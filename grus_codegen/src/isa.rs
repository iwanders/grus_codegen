use cranelift_codegen::ir::{self, Function, InstructionData};
use regalloc2::Inst as RegInst;

use anyhow::Context;
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
impl Default for X86Isa {
    fn default() -> Self {
        Self::new()
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

        // Call convention is kinda hardcoded on the regalloc side...
        // reg0 is arg 0; EDI, Diane's
        // reg1 is arg 1; ESI, Silk
        // reg2 is arg 2; EDX, Dress
        // reg3 is arg 3; ECX, Costs
        // reg4 is arg 4; r8?, 8
        // reg5 is arg 5; r9?, 9$

        // And regalloc uses 'n' registers from 0..n.
        // So we need to map those to something.
        // let limit_regs_4 = true;
        let regmap = [
            Reg::EDI, // PReg(0),
            Reg::ESI, // PReg(1),
            Reg::EDX, // PReg(2),
            Reg::ECX, // PReg(3),
                      // Reg::R8,  // PReg(4),
                      // Reg::R9,  // PReg(5),
                      // Reg::R10, Reg::R11, Reg::R12, Reg::R13, Reg::R14, Reg::R15
        ];
        let rg2x = |p: regalloc2::PReg| regmap[p.index()];
        // let scratch = Reg::EBX;

        let lirfun = crate::lir::Function::from_ir(&func);

        let regs = grus_regalloc::run(
            &lirfun.reg_wrapper(),
            &grus_regalloc::simple_int_machine(4, 0),
        )?;
        debug!(" regs: {regs:#?}");

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
                let arguments = instdata.arguments(&dfg.value_lists);
                let type_of = |v: &ir::Value| dfg.value_type(*v);
                let types_of =
                    |v: &[ir::Value]| v.iter().map(|z| dfg.value_type(*z)).collect::<Vec<_>>();
                debug!("  args: {:?} types: {:?}", arguments, types_of(arguments));

                let allocs_args = regs.inst_allocs(RegInst::new(inst.as_u32() as usize));
                debug!("  allocs: {allocs_args:?}");
                let use_allocs = &allocs_args[0..arguments.len()];
                debug!("  use_allocs: {use_allocs:?}");
                let def_allocs = &allocs_args[arguments.len().min(arguments.len() + 1)..];
                debug!("  def_allocs: {def_allocs:?}");

                debug!(
                    "  results? {:?} -> {:?}  (types: {:?}) ",
                    dfg.has_results(inst),
                    dfg.inst_results(inst),
                    types_of(dfg.inst_results(inst))
                );
                let typevar_operand = instdata.typevar_operand(&dfg.value_lists);
                debug!(
                    "  typevar_operand: {:?}, type: {:?}",
                    typevar_operand,
                    typevar_operand.as_ref().map(type_of)
                );
                debug!("  opcode: {:?}", instdata.opcode());

                match instdata {
                    InstructionData::UnaryImm { opcode, imm } => match opcode {
                        ir::Opcode::Iconst => {
                            let xinst = cg::Instruction::op(
                                Op::Mov(Width::W64),
                                &[
                                    Operand::Reg(rg2x(def_allocs[0].as_reg().unwrap())),
                                    Operand::Immediate(imm.bits()),
                                ],
                            );
                            buffer.append(&mut xinst.serialize()?);
                        }
                        _ => todo!(
                            "unimplemented opcode: {:?} in {:?}, of {:?}",
                            opcode,
                            b,
                            func.name
                        ),
                    },
                    InstructionData::MultiAry { opcode, args } => match opcode {
                        ir::Opcode::Return => {
                            println!("  Return  args: {args:?}");
                            let xinst = cg::Instruction::op(
                                Op::Mov(Width::W64),
                                &[
                                    Operand::Reg(Reg::EAX),
                                    Operand::Reg(rg2x(use_allocs[0].as_reg().unwrap())),
                                ],
                            );
                            buffer.append(&mut xinst.serialize()?);
                            buffer.append(&mut cg::Instruction::op(Op::Return, &[]).serialize()?);
                        }
                        _ => todo!(
                            "unimplemented opcode: {:?} in {:?}, of {:?}",
                            opcode,
                            b,
                            func.name
                        ),
                    },
                    InstructionData::Binary { opcode, args: _ } => match opcode {
                        ir::Opcode::Iadd => {
                            let width: Width = typevar_operand
                                .as_ref()
                                .map(type_of)
                                .with_context(|| "could not determine width")?
                                .into();

                            // x86 add overwrites the first operand, so for now:
                            //    Move operand one into destination.
                            //    Add operand two to destination.
                            let dest = Operand::Reg(rg2x(def_allocs[0].as_reg().unwrap()));
                            let xinst = cg::Instruction::op(
                                Op::Mov(width),
                                &[dest, Operand::Reg(rg2x(use_allocs[0].as_reg().unwrap()))],
                            );
                            buffer.append(&mut xinst.serialize()?);
                            let xinst = cg::Instruction::op(
                                Op::IAdd(width),
                                &[dest, Operand::Reg(rg2x(use_allocs[1].as_reg().unwrap()))],
                            );
                            buffer.append(&mut xinst.serialize()?);
                        }
                        ir::Opcode::Isub => {
                            let width: Width = typevar_operand
                                .as_ref()
                                .map(type_of)
                                .with_context(|| "could not determine width")?
                                .into();

                            // x86 add overwrites the first operand, so for now:
                            //    Move operand one into destination.
                            //    Add operand two to destination.
                            let dest = Operand::Reg(rg2x(def_allocs[0].as_reg().unwrap()));
                            let xinst = cg::Instruction::op(
                                Op::Mov(width),
                                &[dest, Operand::Reg(rg2x(use_allocs[0].as_reg().unwrap()))],
                            );
                            buffer.append(&mut xinst.serialize()?);
                            let xinst = cg::Instruction::op(
                                Op::ISub(width),
                                &[dest, Operand::Reg(rg2x(use_allocs[1].as_reg().unwrap()))],
                            );
                            buffer.append(&mut xinst.serialize()?);
                        }
                        ir::Opcode::Imul => {
                            let width: Width = typevar_operand
                                .as_ref()
                                .map(type_of)
                                .with_context(|| "could not determine width")?
                                .into();

                            // x86 integer mul supports three forms, we use the two-operand
                            // version here, this multiplies into the destination.
                            let dest = Operand::Reg(rg2x(def_allocs[0].as_reg().unwrap()));
                            let xinst = cg::Instruction::op(
                                Op::Mov(width),
                                &[dest, Operand::Reg(rg2x(use_allocs[0].as_reg().unwrap()))],
                            );
                            buffer.append(&mut xinst.serialize()?);
                            let xinst = cg::Instruction::op(
                                Op::IMul(width),
                                &[dest, Operand::Reg(rg2x(use_allocs[1].as_reg().unwrap()))],
                            );
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
                        "unimplemented instructiondata: {:?} in {:?}, of {:?}",
                        instdata,
                        b,
                        func.name
                    ),
                }
            }
        }
        let mut s = String::new();
        for b in buffer.iter() {
            s += &format!(" 0x{b:0>2x}");
        }
        warn!("buffer: echo {s}");

        Ok(CompiledCode {
            buffer,
            // Size of stack frame, in bytes.
            // frame_size,
        })
    }
}
