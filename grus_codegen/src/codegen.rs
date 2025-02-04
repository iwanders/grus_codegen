use cranelift::prelude::Imm64;
use log::warn;
// use smallvec::{SmallVec, ToSmallVec};
use arrayvec::ArrayVec;

use thiserror::Error;

/*
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
    Add(Width),
    Return,
}

impl Op {
    pub fn operand_range(&self) -> OperandRange {
        match self {
            Op::Mov(_) => 2..=2,
            Op::Add(_) => 2..=2,
            _ => 0..=0,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Reg(u8);
impl Reg {
    pub const RAX: Reg = Reg(0);
    pub fn index(&self) -> u8 {
        self.0
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    /// A direct register.
    Reg(Reg),
    /// An immediate value.
    Immediate(Imm64),
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Width {
    W64,
    W32,
    W16,
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
            operands: operands.iter().map(|z| *z).collect(),
        }
    }

    // Fig 2-4, 2.2.1.1 of 325383-sdm-vol-2abcd-dec-24.pdf
    fn addr_mem(r: Reg, b: Reg, width: Width) -> (Rex, ModRM) {
        todo!()
    }
    // Fig 2-6, 2.2.1.1 of 325383-sdm-vol-2abcd-dec-24.pdf
    fn addr_reg(r: Reg, opcode: &[u8], width: Width) -> Result<(Rex, OpcodeVec), CodegenError> {
        let mut rex: u8 = 0b0100 << 4;
        if r.index() > 0b1111 {
            return Err(CodegenError::InvalidRegister { reg: r }.into());
        }
        let top = (r.index() & 0b1111) >> 3;
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
            }
            .into());
        }

        match self.op {
            Op::Mov(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {}
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        // Use register is in opcode. 16: 'B8+ rw iw', 32: 'B8+ rd id', 64: 'REX.W + B8+ rd io'
                        let (rex, opcode) = Self::addr_reg(r, &[0xb8], width)?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend(b.bits().to_le_bytes());
                    }
                    _ => todo!(),
                }
            }
            Op::Add(width) => {
                let dest = self.operands[0];
                let src = self.operands[1];
                match (dest, src) {
                    (Operand::Reg(r), Operand::Reg(b)) => {}
                    (Operand::Reg(r), Operand::Immediate(b)) => {
                        // Use register is in opcode. 16: 'B8+ rw iw', 32: 'B8+ rd id', 64: 'REX.W + B8+ rd io'
                        let (rex, opcode) = Self::addr_reg(r, &[0xb8], Width::W64)?;
                        v.push(rex.into());
                        v.extend(opcode.iter());
                        v.extend(b.bits().to_le_bytes());
                    }
                    _ => todo!(),
                }
            }
            Op::Return => {
                const RETN: u8 = 0xc3;
                v.push(RETN);
            }
        }
        Ok(v)
    }
}
