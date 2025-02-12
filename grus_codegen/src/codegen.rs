use arrayvec::ArrayVec;
use log::*;
use thiserror::Error;

/*
-output-asm-variant=1 switches to Intel syntax; destination before source.

echo "0x0f 0x28 0x44 0xd8 0x10" | llvm-mc-13  -disassemble -triple=x86_64 -output-asm-variant=1
echo "0x48 0xb8 0x88 0x77 0x66 0x55 0x44 0x33 0x22 0x11" | llvm-mc-13  -disassemble -triple=x86_64 -output-asm-variant=1

325383-sdm-vol-2abcd-dec-24.pdf
    p 39. contains REX & ModRM Byte diagram.

    p35 DI & SI appears to be special in 16 bit... lets just ignore that?
*/

type OperandRange = std::ops::RangeInclusive<usize>;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("register invalid: {reg:?}")]
    InvalidRegister { reg: Reg },
    #[error("invalid operand count: got {got} expected {expected:?}")]
    InvalidOperandCount { got: usize, expected: OperandRange },
}

#[derive(Debug, Copy, Clone)]
pub enum Op {
    Mov(Width),
    IAdd(Width),
    ISub(Width),
    IMul(Width),
    Return,
}

impl Op {
    pub fn operand_range(&self) -> OperandRange {
        match self {
            Op::Mov(_) => 2..=2,
            Op::IAdd(_) => 2..=2,
            Op::ISub(_) => 2..=2,
            Op::IMul(_) => 2..=2, // heh, technically 1..=3... with 3 only with intermediate, 1 for eax
            Op::Return => 0..=0,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Reg(u8);
impl Reg {
    pub const EAX: Reg = Reg(0b000);
    pub const ECX: Reg = Reg(0b001);
    pub const EDX: Reg = Reg(0b010);
    pub const EBX: Reg = Reg(0b011);
    pub const EBP: Reg = Reg(0b101);
    pub const ESP: Reg = Reg(0b100);
    pub const ESI: Reg = Reg(0b110);
    pub const EDI: Reg = Reg(0b111);

    // And the 64 bits extended registers.
    pub const R8: Reg = Reg(0b1000);
    pub const R9: Reg = Reg(0b1001);
    pub const R10: Reg = Reg(0b1010);
    pub const R11: Reg = Reg(0b1011);
    pub const R12: Reg = Reg(0b1101);
    pub const R13: Reg = Reg(0b1100);
    pub const R14: Reg = Reg(0b1110);
    pub const R15: Reg = Reg(0b1111);

    pub fn index(&self) -> u8 {
        self.0
    }
    pub fn from(v: u8) -> Self {
        Reg(v)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    /// A direct register.
    Reg(Reg),
    /// An immediate value.
    Immediate(i64),
    // TODO: Magic stuff [EAX] and [--][--], page 36.
    // Memory(Reg)?
    // MemoryOffset(RegBase, RegOffset)?
}

pub type OperandVec = ArrayVec<Operand, 4>;

#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: Op,
    pub operands: OperandVec,
}

#[derive(Debug, Copy, Clone)]
pub struct Rex(u8);
impl From<Rex> for u8 {
    fn from(v: Rex) -> Self {
        v.0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ModRM(u8);
impl From<ModRM> for u8 {
    fn from(v: ModRM) -> Self {
        v.0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Width {
    W128,
    W64,
    W32,
    W16,
    W8,
}
impl From<cranelift_codegen::ir::types::Type> for Width {
    fn from(v: cranelift_codegen::ir::Type) -> Self {
        match v.bytes() {
            val if val == 128 / 8 => Width::W128,
            val if val == 64 / 8 => Width::W64,
            val if val == 32 / 8 => Width::W32,
            val if val == 16 / 8 => Width::W16,
            1 => Width::W8,
            _ => panic!("unhandled width: {}", v.bytes()),
        }
    }
}

impl Width {
    fn rex_bit(&self) -> u8 {
        if *self == Width::W64 {
            0b1000
        } else {
            0
        }
    }
}

pub type OpcodeVec = ArrayVec<u8, 3>;

impl Instruction {
    pub fn op(op: Op, operands: &[Operand]) -> Self {
        Self {
            op,
            operands: operands.iter().copied().collect(),
        }
    }

    // Fig 2-5, 2.2.1.1 of 325383-sdm-vol-2abcd-dec-24.pdf
    /// Format register r (R) as first argument, b as second argument (depicted M) in intel manual.
    fn addr_regs(r: Reg, b: Reg, width: Width) -> Result<(Rex, ModRM), CodegenError> {
        let mut rex: u8 = 0b0100 << 4;
        let rtop = (r.index() & 0b1111) >> 3;
        let rlower = r.index() & 0b111;
        let btop = (b.index() & 0b1111) >> 3;
        let blower = b.index() & 0b111;
        rex |= btop;
        rex |= rtop << 2;
        rex |= width.rex_bit();
        let mut modrm: u8 = 0;
        modrm |= blower;
        modrm |= rlower << 3;
        modrm |= 0b11 << 6;
        // what goes into the left bits??
        Ok((Rex(rex), ModRM(modrm)))
    }

    // Fig 2-7, 2.2.1.1 of 325383-sdm-vol-2abcd-dec-24.pdf
    fn addr_reg(r: Reg, opcode: &[u8], width: Width) -> Result<(Rex, OpcodeVec), CodegenError> {
        let mut rex: u8 = 0b0100 << 4;
        if r.index() > 0b1111 {
            return Err(CodegenError::InvalidRegister { reg: r });
        }
        let top = (r.index() & 0b1111) >> 3;
        rex |= top;
        let lower = r.index() & 0b111;
        rex |= width.rex_bit();
        let mut z: OpcodeVec = opcode.iter().copied().collect();
        *z.last_mut().unwrap() |= lower;

        Ok((Rex(rex), z))
    }

    // Seriously inefficient
    pub fn serialize(&self) -> Result<Vec<u8>, CodegenError> {
        /*
            rex is 0b0100WR0B
                w: 0=operand size by CS.D, 1 is 64 bit., whatever that may mean.
                R: Extension of the ModRM reg field.
                X: Extension of the SIB index field.
                B: Extension of the ModRM r/m field, SIB base field, or Opcode reg field.
            See page 39, fig 2-4, 2-5, 2-6...
            There's 4 flavours;
                fig 2.4: Memory without SIB
                fig 2.5: Register-register, no memory.
                fig 2.6: Memory with SIB
                fig 2.7: Register operand coded in opcode byte.

            should probably study p104 of 325383-sdm-vol-2abcd-dec-24
        */
        let mut v = vec![];

        // Validate operand count.
        let expected = self.op.operand_range();
        if !expected.contains(&self.operands.len()) {
            return Err(CodegenError::InvalidOperandCount {
                got: self.operands.len(),
                expected,
            });
        }

        match self.op {
            Op::Mov(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {
                        let (rex, modrm) = Self::addr_regs(r, b, width)?;
                        // Copies second into first.
                        v.push(rex.into());
                        v.push(0x8B);
                        v.push(modrm.into());
                    }
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        // Use register is in opcode. MOV 16: 'B8+ rw iw', 32: 'B8+ rd id', 64: 'REX.W + B8+ rd io'
                        let (rex, opcode) = Self::addr_reg(r, &[0xB8], width)?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend(b.to_le_bytes());
                    }
                    _ => todo!(),
                }
            }
            Op::IAdd(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {
                        // // /r, ADD, 16: '01 /r', 32: '01 /r', 64: 'REX.W + 01 /r'
                        // Add destination (first) to second, then stores result in destination.
                        let (rex, modrm) = Self::addr_regs(r, b, width)?;
                        v.push(rex.into());
                        v.push(0x03);
                        v.push(modrm.into());
                    }
                    (Operand::Reg(_r), Operand::Immediate(_b)) => {
                        todo!()
                    }
                    _ => todo!(),
                }
            }
            Op::ISub(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {
                        // Sub destination (first) to second, then stores result in destination.
                        let (rex, modrm) = Self::addr_regs(r, b, width)?;
                        v.push(rex.into());
                        v.push(0x2B);
                        v.push(modrm.into());
                    }
                    (Operand::Reg(_r), Operand::Immediate(_b)) => {
                        todo!()
                    }
                    _ => todo!(),
                }
            }
            Op::IMul(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {
                        // Mul destination (first) to second, then stores result in destination.
                        let (rex, modrm) = Self::addr_regs(r, b, width)?;
                        v.push(rex.into());
                        v.push(0x0F);
                        v.push(0xAF);
                        v.push(modrm.into());
                    }
                    (Operand::Reg(_r), Operand::Immediate(_b)) => {
                        todo!()
                    }
                    _ => todo!(),
                }
            }
            Op::Return => {
                const RETN: u8 = 0xC3;
                v.push(RETN);
            }
        }
        Ok(v)
    }
}
