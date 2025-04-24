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

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy, Clone)]
pub enum Condition {
    /// Jump if, ZF == 0.
    IfNotEqual,
    /// Jump if, ZF != 0.
    IfEqual,
    /// If Less, SF != OF
    IfLess,
    /// If Greater if greater (ZF=0 and SF=OF).
    IfGreater,
}

/// x86 instructions
///
#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum Op {
    /// MOV - Move
    ///
    /// MOV r16,r/m16   MOV r32,r/m32  MOV r64,r/m64
    Mov(Width),
    /// ADD - Add
    ///
    /// ADD r16, r/m16   ADD r32, r/m32  ADD r64, r/m64
    IAdd(Width),
    /// SUB - Subtract
    ///
    /// SUB r16, r/m16   SUB r32, r/m32  SUB r64, r/m64
    ISub(Width),
    /// IMUL - Signed Multiply
    ///
    /// IMUL r16,r/m16   IMUL r32,r/m32  IMUL r64,r/m64
    IMul(Width),
    /// And - Bitwise And
    ///
    And(Width),
    /// RET - Return from procedure
    Return,
    /// TEST - Logical Compare
    ///
    /// TEST r/m16, imm16   TEST r/m32, imm32   TEST r/m64, imm64
    /// TEST r/m16, r16   TEST r/m32, r32   TEST r/m64, r64
    Test(Width),

    /// CMP - Compare
    ///
    /// REX.W + 3B /r CMP r64, r/m64 RM Valid N.E. Compare r/m64 with r64.
    Cmp(Width),

    /// JCC - Jump if Condition is Met
    ///
    ///
    Jcc(Condition),

    /// SetCC - Conditionally set a register to 0 or 1
    ///
    ///
    SetCC(Condition),

    /// NOP - No Operation
    ///
    /// Does nothing except advance the EIP register.
    Nop,

    /// Jump, Somewhere
    ///
    /// Relative offset from current instruction position.
    Jump,

    /// Interrupt 3
    ///
    /// Generate breakpoint trap.
    Int3,

    /// Push - Push a value onto the stack
    Push,

    /// Pop - Pop a value from the stack
    Pop,
}

impl Op {
    pub fn operand_range(&self) -> OperandRange {
        match self {
            Op::Mov(_) => 2..=2,
            Op::IAdd(_) => 2..=2,
            Op::ISub(_) => 2..=2,
            Op::And(_) => 2..=2,
            Op::IMul(_) => 2..=2, // heh, technically 1..=3... with 3 only with intermediate, 1 for eax
            Op::Return => 0..=0,
            Op::Test(_) => 2..=2,
            Op::Cmp(_) => 2..=2,
            Op::Jcc(_) => 1..=1,
            Op::SetCC(_) => 1..=1,
            Op::Nop => 0..=0,
            Op::Jump => 1..=1,
            Op::Int3 => 0..=0,
            Op::Push => 1..=1,
            Op::Pop => 1..=1,
        }
    }
    pub fn is_return(&self) -> bool {
        match self {
            Op::Return => true,
            _ => false,
        }
    }
    pub fn is_branch(&self) -> bool {
        match self {
            Op::Jcc(_) => true,
            Op::Jump => true,
            _ => false,
        }
    }
}

/*
Todo, change registers to u64
    Then we can express each register as a single bit
    And easily specify which registers may (or may not) be used by a particular instruction.
*/
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

pub const CALLING_CONVENTION_REGISTERS: [Reg; 6] =
    [Reg::EDI, Reg::ESI, Reg::EDX, Reg::ECX, Reg::R8, Reg::R9];

#[derive(Debug, Copy, Clone)]
pub enum Offset {
    /// Offset by a register value and a scaling of that value.
    Reg { reg: Reg, scale: usize },
    /// An immediate value.
    Immediate(i64),
}
impl Offset {
    pub fn as_immediate(&self) -> Option<i64> {
        match *self {
            Offset::Immediate(a) => Some(a),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RegOffset {
    pub reg: Reg,
    pub offset: Offset,
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
    //
    RegOffset(RegOffset),
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
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

// These are here because i tis sometimes helpful to insert one of these during debugging.
const RETN: u8 = 0xC3;
const INT3: u8 = 0xCC;
const NOP: u8 = 0x90;

pub enum ModSpec {
    Memory, // 0b00
    MemoryExtension(u8),
    MemoryDisp8,  // 0b01
    MemoryDisp32, // 0b10
    MemoryDisp32Extension(u8),
    RegisterRegister, // 0b11,
    RegisterExtension(u8),
}

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
    fn addr_reg(
        r: Reg,
        opcode: &[u8],
        width: Width,
        modspec: ModSpec,
    ) -> Result<(Rex, OpcodeVec), CodegenError> {
        let mut rex: u8 = 0b0100 << 4;
        if r.index() > 0b1111 {
            return Err(CodegenError::InvalidRegister { reg: r });
        }
        let top = (r.index() & 0b1111) >> 3;
        rex |= top;
        let lower = r.index() & 0b111;
        rex |= width.rex_bit();
        let mut z: OpcodeVec = opcode.iter().copied().collect();
        let lb = z.last_mut().unwrap();
        *lb |= lower;
        *lb |= match modspec {
            ModSpec::Memory | ModSpec::MemoryExtension(_) => 0b00,
            ModSpec::MemoryDisp8 => 0b01,
            ModSpec::MemoryDisp32 => 0b10,
            ModSpec::MemoryDisp32Extension(_) => 0b10,
            ModSpec::RegisterRegister => 0b11,
            ModSpec::RegisterExtension(_) => 0b11,
        } << 6;

        match modspec {
            ModSpec::MemoryExtension(v) => {
                // v goes into register slot of modrm
                if v > 7 {
                    panic!("got an instruction index exceeding 7");
                }
                rex |= ((v >> 3) & 0b1) << 2;
                *lb |= (v & 0b111) << 4;
            }
            ModSpec::RegisterExtension(v) => {
                if v > 7 {
                    panic!("got an instruction index exceeding 7");
                }
                rex |= ((v >> 3) & 0b1) << 2;
                *lb |= (v & 0b111) << 3;
            }

            ModSpec::MemoryDisp32Extension(v) => {
                if v > 7 {
                    panic!("got an instruction index exceeding 7");
                }
                rex |= ((v >> 3) & 0b1) << 2;
                *lb |= (v & 0b111) << 3;
            }
            _ => {}
        }

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
                        let (rex, opcode) = Self::addr_reg(r, &[0xB8], width, ModSpec::Memory)?;
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
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        // Move immediate into destination, then do a normal addition based on register by register.
                        if (b as i32) as i64 != b {
                            todo!("need a scratch register if we need more than 32 bits");
                        }
                        let (rex, opcode) =
                            Self::addr_reg(r, &[0x81, 0], width, ModSpec::RegisterRegister)?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend((b as u32).to_le_bytes());
                    }

                    (Operand::RegOffset(reg_offset), Operand::Immediate(b)) => {
                        let (rex, opcode) = Self::addr_reg(
                            reg_offset.reg,
                            &[0x81, 0],
                            Width::W64,
                            ModSpec::MemoryDisp32Extension(0),
                        )?;
                        v.push(rex.into());
                        v.extend(opcode.iter());

                        v.extend(
                            (reg_offset
                                .offset
                                .as_immediate()
                                .expect("unhandled reg offset")
                                as i32)
                                .to_le_bytes(),
                        );
                        v.extend((b as u32).to_le_bytes());
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
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        let (rex, opcode) =
                            Self::addr_reg(r, &[0x81, 0x00], width, ModSpec::RegisterExtension(5))?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend((b as i32).to_le_bytes());
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
            Op::And(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(_r), Operand::Reg(_b)) => {
                        todo!();
                    }
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        let (rex, opcode) =
                            Self::addr_reg(r, &[0x81, 0x00], width, ModSpec::RegisterExtension(4))?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend((b as i32).to_le_bytes());
                    }
                    (Operand::RegOffset(reg_offset), Operand::Immediate(b)) => {
                        let (rex, opcode) = Self::addr_reg(
                            reg_offset.reg,
                            &[0x81, 0],
                            width,
                            ModSpec::MemoryDisp32Extension(4),
                        )?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend(
                            (reg_offset
                                .offset
                                .as_immediate()
                                .expect("unhandled reg offset")
                                as i32)
                                .to_le_bytes(),
                        );
                        v.extend((b as u32).to_le_bytes());
                    }
                    _ => todo!(),
                }
            }
            Op::Return => {
                v.push(RETN);
            }
            Op::Test(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(_r), Operand::Reg(_b)) => {
                        todo!()
                    }
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        if width == Width::W8 {
                            todo!(); // something special with AH, BH,CH, DH
                        }

                        let (rex, opcode) =
                            Self::addr_reg(r, &[0xF7, 0], width, ModSpec::RegisterRegister)?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend((b as u32).to_le_bytes());
                    }

                    (Operand::RegOffset(reg_offset), Operand::Immediate(b)) => {
                        let (rex, opcode) = Self::addr_reg(
                            reg_offset.reg,
                            &[0xF7, 0],
                            width,
                            ModSpec::MemoryDisp32,
                        )?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend(
                            (reg_offset
                                .offset
                                .as_immediate()
                                .expect("unhandled reg offset")
                                as i32)
                                .to_le_bytes(),
                        );
                        v.extend((b as u32).to_le_bytes());
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            Op::Cmp(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {
                        let (rex, modrm) = Self::addr_regs(r, b, width)?;
                        v.push(rex.into());
                        v.push(0x3B);
                        v.push(modrm.into());
                    }
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        if width == Width::W8 {
                            todo!(); // something special with AH, BH,CH, DH
                        }

                        let (rex, opcode) =
                            Self::addr_reg(r, &[0x81, 0], width, ModSpec::RegisterExtension(7))?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend((b as u32).to_le_bytes());
                    }
                    (Operand::RegOffset(reg_offset), Operand::Immediate(b)) => {
                        let (rex, opcode) = Self::addr_reg(
                            reg_offset.reg,
                            &[0x81, 0],
                            Width::W64,
                            ModSpec::MemoryDisp32Extension(7),
                        )?;
                        v.push(rex.into());
                        v.extend(opcode.iter());

                        v.extend(
                            (reg_offset
                                .offset
                                .as_immediate()
                                .expect("unhandled reg offset")
                                as i32)
                                .to_le_bytes(),
                        );
                        v.extend((b as u32).to_le_bytes());
                    }
                    _ => todo!(),
                }
            }
            Op::SetCC(cond) => {
                let r = self.operands[0];
                let opcode = match cond {
                    Condition::IfLess => {
                        const SET_BYTE_IF_LESS_SF_NE_OF: [u8; 3] = [0x0F, 0x9C, 0x00];
                        SET_BYTE_IF_LESS_SF_NE_OF
                    }
                    Condition::IfGreater => {
                        const SET_BYTE_IF_GREATER_ZF_EQ_0_AND_SF_EQ_OF: [u8; 3] =
                            [0x0F, 0x9F, 0x00];
                        SET_BYTE_IF_GREATER_ZF_EQ_0_AND_SF_EQ_OF
                    }

                    _ => {
                        todo!("handle cond: {cond:?}");
                    }
                };
                match r {
                    Operand::Reg(r) => {
                        let (rex, opcode) =
                            Self::addr_reg(r, &opcode, Width::W8, ModSpec::RegisterRegister)?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                    }
                    Operand::Immediate(_) => {
                        panic!("got immediate for setCC destination");
                    }
                    Operand::RegOffset(reg_offset) => {
                        let (rex, opcode) = Self::addr_reg(
                            reg_offset.reg,
                            &opcode,
                            Width::W8,
                            ModSpec::MemoryDisp32,
                        )?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend(
                            (reg_offset
                                .offset
                                .as_immediate()
                                .expect("unhandled reg offset")
                                as i32)
                                .to_le_bytes(),
                        );
                    }
                }
            }
            Op::Jcc(cond) => {
                let offset = self.operands[0];
                let opcode = match cond {
                    Condition::IfEqual => {
                        const JUMP_NEAR_IF_NOT_EQUAL_ZF_EQ_0_REL32: [u8; 2] = [0x0F, 0x85];
                        JUMP_NEAR_IF_NOT_EQUAL_ZF_EQ_0_REL32
                    }
                    Condition::IfNotEqual => {
                        const JUMP_NEAR_IF_NOT_EQUAL_ZF_NE_0_REL32: [u8; 2] = [0x0F, 0x84];
                        JUMP_NEAR_IF_NOT_EQUAL_ZF_NE_0_REL32
                    }
                    _ => {
                        todo!("handle cond: {cond:?}");
                    }
                };
                if let Operand::Immediate(value) = offset {
                    v.extend(opcode);
                    v.extend((value as i32).to_le_bytes());
                }
            }
            Op::Jump => {
                let offset = self.operands[0];
                const JUMP_REL32: u8 = 0xE9;
                if let Operand::Immediate(value) = offset {
                    v.push(JUMP_REL32);
                    v.extend((value as i32).to_le_bytes());
                }
            }
            Op::Push => {
                //v.push(INT3);
                let r = self.operands[0];
                const PUSH_R64: u8 = 0x50;
                if let Operand::Reg(r) = r {
                    v.push(PUSH_R64 + r.index());
                } else {
                    todo!();
                }
            }

            Op::Pop => {
                let r = self.operands[0];
                const POP_R64: u8 = 0x58;
                if let Operand::Reg(r) = r {
                    v.push(POP_R64 + r.index());
                } else {
                    todo!();
                }
            }
            Op::Nop => {
                v.push(NOP);
            }
            Op::Int3 => {
                v.push(INT3);
            }
        }
        Ok(v)
    }
}

#[cfg(test)]
mod test {

    fn hexstring(buffer: &[u8]) -> String {
        let mut s = String::new();
        for b in buffer.iter() {
            s += &format!(" 0x{b:0>2x}");
        }
        s
    }

    fn bitstring(buffer: &[u8]) -> String {
        let mut s = String::new();
        for b in buffer.iter() {
            s += &format!(" 0b{b:0>8b}");
        }
        s
    }
    use super::*;
    #[test]
    fn test_addition() {
        let instructions_expectation: Vec<(Instruction, &[u8])> = vec![
            (
                Instruction::op(
                    Op::IAdd(Width::W64),
                    &[Operand::Reg(Reg::EBX), Operand::Reg(Reg::ECX)],
                ),
                &[0x48, 0x03, 0xd9],
            ),
            (
                Instruction::op(
                    Op::IAdd(Width::W64),
                    &[Operand::Reg(Reg::R11), Operand::Reg(Reg::EDX)],
                ),
                &[0x4c, 0x03, 0xda],
            ),
            (
                Instruction::op(
                    Op::IAdd(Width::W64),
                    &[Operand::Reg(Reg::R11), Operand::Immediate(1337)],
                ),
                &[0x49, 0x81, 0xc3, 0x39, 0x05, 0x00, 0x00],
            ),
            (
                Instruction::op(
                    Op::IAdd(Width::W64),
                    &[
                        Operand::RegOffset(RegOffset {
                            reg: Reg::EBP,
                            offset: Offset::Immediate(8),
                        }),
                        Operand::Immediate(1337),
                    ],
                ),
                &[
                    0x48, 0x81, 0x85, 0x08, 0x00, 0x00, 0x00, 0x39, 0x05, 0x00, 0x00,
                ],
            ),
        ];
        for (instruction, expectation) in instructions_expectation {
            let serialized = instruction.serialize().unwrap();
            println!(
                "{instruction:?}:  {}  {}",
                hexstring(&serialized),
                bitstring(&serialized)
            );
            assert_eq!(&serialized, expectation);
        }
    }
}
