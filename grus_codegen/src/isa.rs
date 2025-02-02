use cranelift_codegen::ir::{self, Function, InstructionData};
use cranelift_codegen::CodegenResult;

use log::*;
use target_lexicon::Triple;

use thiserror::Error;
#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Register invalid: {reason} in {operand:?}")]
    InvalidOperand {
        reason: String,
        operand: x86::Operand,
    },
}

#[derive(Debug)]
pub struct X86Isa {
    triple: Triple,
}

impl X86Isa {
    pub fn new() -> Self {
        Self {
            triple: target_lexicon::triple!("x86_64"),
        }
    }
}

#[derive(Debug)]
pub struct CompiledCode {
    pub buffer: Vec<u8>,
    // pub frame_size: usize,
}

impl X86Isa {
    pub fn name(&self) -> &'static str {
        "X86Isa"
    }
    pub fn triple(&self) -> &Triple {
        &self.triple
    }

    pub fn compile_function(&self, func: &Function) -> Result<CompiledCode, CodegenError> {
        let _ = func;

        // Okay, so now we get a stencil, that has a dfg, and we need to output instructions for that.
        let stencil = &func.stencil;
        // https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/function/struct.FunctionStencil.html

        // If we encounter any of these, we don't support that yet.
        if !stencil.sized_stack_slots.is_empty() {
            todo!("dynamic_stack_slots")
        }
        if !stencil.dynamic_stack_slots.is_empty() {
            todo!("dynamic_stack_slots")
        }
        if !stencil.global_values.is_empty() {
            todo!("global_values")
        }
        if !stencil.global_value_facts.is_empty() {
            todo!("global_value_facts")
        }

        /*
        Okay, now we need to do something with the dfg and layout

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/layout/struct.Layout.html
        > The Layout struct determines the layout of blocks and instructions in a function

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/dfg/struct.DataFlowGraph.html

        A data flow graph defines all instructions and basic blocks in a function as well as the
        data flow dependencies between them. The DFG also tracks values which can be either
        instruction results or block parameters.

        The layout of blocks in the function and of instructions in each block is recorded by the
        Layout data structure which forms the other half of the function representation.

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/instructions/enum.InstructionData.html
        */

        let dfg = &stencil.dfg;
        let layout = &stencil.layout;

        let mut buffer = vec![];

        for b in layout.blocks() {
            debug!("b: {b:?}");
            let block_data = &dfg.blocks[b];
            debug!(
                "block_data params: {:?}",
                block_data.params(&dfg.value_lists)
            );
            for inst in layout.block_insts(b) {
                debug!("inst: {inst:?}");
                let instdata = dfg.insts[inst];
                debug!("  instruction_data: {instdata:?}");
                debug!(
                    "  typevar_operand: {:?}",
                    instdata.typevar_operand(&dfg.value_lists)
                );
                let arguments = instdata.arguments(&dfg.value_lists);
                debug!("  args: {:?}", arguments);
                debug!("  opcode: {:?}", instdata.opcode());
                debug!(
                    "  results? {:?} -> {:?} ",
                    dfg.has_results(inst),
                    dfg.inst_results(inst)
                );
                // We also don't have the types here... do WE have to propagate those?
                match instdata {
                    InstructionData::UnaryImm { opcode, imm } => {
                        // How do we know where this goes..
                        // Line is `v1 = iconst.i8 0` in the clif... how do we get the Value of v1?
                        // Immediate to register, p2487
                        match opcode {
                            ir::Opcode::Iconst => {
                                // 0b1011W000  W is 64??
                                let xinst = x86::Instruction::op_1(
                                    &[0b10111000],
                                    x86::Operand::Reg(x86::Reg(0)),
                                )
                                .with_immediate(x86::Operand::Immediate(imm));
                                buffer.append(&mut xinst.serialize()?);
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                b,
                                func.name
                            ),
                        }
                    }
                    InstructionData::MultiAry { opcode, args } => match opcode {
                        ir::Opcode::Return => {
                            let xinst = x86::Instruction::op_0(&[0xc3]);
                            buffer.append(&mut xinst.serialize()?);
                        }
                        _ => todo!(
                            "unimplemented opcode: {:?} in {:?}, of {:?}",
                            opcode,
                            b,
                            func.name
                        ),
                    },
                    _ => todo!(
                        "unimplemented: {:?} in {:?}, of {:?}",
                        instdata,
                        b,
                        func.name
                    ),
                }
            }
        }
        warn!("buffer: {buffer:x?}");

        // const NOP: u8 = 0x90;
        // const RETN: u8 = 0xc3;
        // const INT3: u8 = 0xcc;
        // let buffer = vec![0xB8, 0x46, 0x00, 0x00, 0x00, NOP, NOP, INT3, RETN];
        // 0xB8, 0x46, 0x00, 0x00, 0x00 is writing 0x46 to EAX
        // let buffer = vec![0xB8, 0x46, 0x00, 0x00, 0x00, NOP, NOP, RETN];
        Ok(CompiledCode {
            buffer,
            // Size of stack frame, in bytes.
            // frame_size,
        })
    }
}

mod x86 {
    use super::*;
    use cranelift::prelude::Imm64;
    /*
    echo "0x0f 0x28 0x44 0xd8 0x10" | llvm-mc-13  -disassemble -triple=x86_64 -output-asm-variant=1
    echo "0x48 0xb8 0x88 0x77 0x66 0x55 0x44 0x33 0x22 0x11" | llvm-mc-13  -disassemble -triple=x86_64 -output-asm-variant=1

    325383-sdm-vol-2abcd-dec-24.pdf
        p 39. contains REX & ModRM Byte diagram.

        p35 DI & SI appears to be special in 16 bit... lets just ignore that?
    */
    #[derive(Debug, Copy, Clone)]
    pub struct Reg(pub u8);

    #[derive(Debug, Copy, Clone)]
    pub enum Operand {
        /// A direct register.
        Reg(Reg),
        Immediate(Imm64),
        // TODO: Magic stuff [EAX] and [--][--], page 36.
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
}
