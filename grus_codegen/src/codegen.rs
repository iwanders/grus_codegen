use cranelift::prelude::Imm64;
use log::warn;

/*
echo "0x0f 0x28 0x44 0xd8 0x10" | llvm-mc-13  -disassemble -triple=x86_64 -output-asm-variant=1
echo "0x48 0xb8 0x88 0x77 0x66 0x55 0x44 0x33 0x22 0x11" | llvm-mc-13  -disassemble -triple=x86_64 -output-asm-variant=1

325383-sdm-vol-2abcd-dec-24.pdf
    p 39. contains REX & ModRM Byte diagram.

    p35 DI & SI appears to be special in 16 bit... lets just ignore that?
*/

#[derive(Debug, Copy, Clone)]
pub enum Op {
    Mov,
}

use thiserror::Error;
#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Register invalid: {reason} in {operand:?}")]
    InvalidOperand { reason: String, operand: Operand },
}

#[derive(Debug, Copy, Clone)]
pub struct Reg(pub u8);

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
#[derive(Debug, Copy, Clone)]
pub enum Operands {
    None,
    Unary(Operand),
    Binary(Operand, Operand),
}

#[derive(Debug, Copy, Clone)]
pub struct Opcode(pub [Option<u8>; 3]);
impl Opcode {
    pub fn from(s: &[u8]) -> Self {
        let mut r: [Option<u8>; 3] = Default::default();
        for (i, v) in s.iter().enumerate() {
            r[i] = Some(*v)
        }

        Self(r)
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut v: Vec<u8> = Vec::with_capacity(2);
        for s in self.0.iter() {
            if let Some(o) = s {
                v.push(*o);
            }
        }
        v
    }
    pub fn serialize_with(&self, lower: u8) -> Vec<u8> {
        let mut v: Vec<u8> = Vec::with_capacity(2);
        for (i, s) in self.0.iter().enumerate() {
            if let Some(o) = s {
                let addition = if self.0.get(i + 1).is_none() {
                    lower
                } else {
                    0
                };
                v.push(*o | lower);
            }
        }
        v
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Operands,
    pub displacement: Option<Operand>,
    pub immediate: Option<Operand>,
}

impl Instruction {
    pub fn op_0(opcode: &[u8]) -> Self {
        Self {
            opcode: Opcode::from(opcode),
            operands: Operands::None,
            displacement: None,
            immediate: None,
        }
    }
    pub fn op_1(opcode: &[u8], operand: Operand) -> Self {
        Self {
            opcode: Opcode::from(opcode),
            operands: Operands::Unary(operand),
            displacement: None,
            immediate: None,
        }
    }

    pub fn op_2(opcode: &[u8], operand1: Operand, operand2: Operand) -> Self {
        Self {
            opcode: Opcode::from(opcode),
            operands: Operands::Binary(operand1, operand2),
            displacement: None,
            immediate: None,
        }
    }

    pub fn with_immediate(self, immediate: Operand) -> Self {
        Self {
            immediate: Some(immediate),
            ..self
        }
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
        let mut rex: u8 = 0b0100 << 4;
        let mut v = vec![];
        let mut opcode_bytes = vec![];
        match &self.operands {
            Operands::None => {
                opcode_bytes = self.opcode.serialize();
            }
            Operands::Unary(operand) => {
                match operand {
                    Operand::Reg(r) => {
                        if r.0 > 0b1111 {
                            return Err(CodegenError::InvalidOperand {
                                reason: "register index too large".to_owned(),
                                operand: *operand,
                            }
                            .into());
                        }
                        let top = (r.0 & 0b1111) >> 3;
                        rex |= top;
                        rex |= 0b1000;
                        let bottom = r.0 & 0b111;
                        // This goes into the opcode, somehow.
                        opcode_bytes = self.opcode.serialize_with(bottom);
                        v.push(rex);
                    }
                    Operand::Immediate(_v) => {}
                }
            }
            Operands::Binary(..) => {
                todo!()
            }
        }
        // let mut v = vec![rex];
        warn!("rex: {rex:x?}");
        v.append(&mut opcode_bytes);
        if let Some(immediate) = self.immediate {
            match immediate {
                Operand::Immediate(imm) => {
                    let vi64 = imm.bits();
                    v.extend(vi64.to_le_bytes());
                }
                _ => todo!(),
            }
        }
        Ok(v)
    }
}
