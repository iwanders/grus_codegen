use cranelift_codegen::ir::{self, Function, InstructionData};
use cranelift_codegen::CodegenResult;

use log::*;
use target_lexicon::Triple;

use crate::codegen as cg;
use cg::{Op, Operand, Reg, Width};

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

    pub fn compile_function(&self, func: &Function) -> Result<CompiledCode, anyhow::Error> {
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
                debug!("  opcode: {:?}", instdata.opcode());
                let arguments = instdata.arguments(&dfg.value_lists);
                let types_of =
                    |v: &[ir::Value]| v.iter().map(|z| dfg.value_type(*z)).collect::<Vec<_>>();
                debug!("  args: {:?} types: {:?}", arguments, types_of(&arguments));

                debug!(
                    "  results? {:?} -> {:?}  (types: {:?}) ",
                    dfg.has_results(inst),
                    dfg.inst_results(inst),
                    types_of(&dfg.inst_results(inst))
                );

                // We also don't have the types here... do WE have to propagate those?
                match instdata {
                    InstructionData::UnaryImm { opcode, imm } => {
                        // How do we know where this goes..
                        // Line is `v1 = iconst.i8 0` in the clif... how do we get the Value of v1?
                        // Immediate to register, p2487
                        match opcode {
                            ir::Opcode::Iconst => {
                                let xinst = cg::Instruction::op(
                                    Op::Mov(Width::W64),
                                    &[Operand::Reg(Reg::RAX), Operand::Immediate(imm)],
                                );
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
                            buffer.append(&mut cg::Instruction::op(Op::Return, &[]).serialize()?);
                        }
                        _ => todo!(
                            "unimplemented opcode: {:?} in {:?}, of {:?}",
                            opcode,
                            b,
                            func.name
                        ),
                    },
                    InstructionData::Binary { opcode, args } => {
                        match opcode {
                            ir::Opcode::Iadd => {
                                // let xinst = cg::Instruction::op_0(&[0xc3]);
                                // buffer.append(&mut xinst.serialize()?);
                                // let xinst = cg::Instruction::op_2(&[0b10111000], cg::Operand::Reg(cg::Reg(0))).with_immediate(cg::Operand::Immediate(imm));
                                // let xinst = cg::Instruction::op(cg::Op::Add(cg::Width::W64), &[cg::Operand::Reg(cg::Reg::RAX), cg::Operand::Immediate(imm)]);
                                // buffer.append(&mut xinst.serialize()?);
                                todo!();
                            }
                            _ => todo!(
                                "unimplemented opcode: {:?} in {:?}, of {:?}",
                                opcode,
                                b,
                                func.name
                            ),
                        }
                    }
                    _ => todo!(
                        "unimplemented instructiondata: {:?} in {:?}, of {:?}",
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
