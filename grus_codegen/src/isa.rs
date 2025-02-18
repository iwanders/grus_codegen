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

        let mut lirfun = crate::lir::Function::from_ir(&func);
        lirfun.lirify();
        lirfun.lower_first();
        let reg_wrapper = lirfun.reg_wrapper();
        println!("{lirfun:#?}");
        let reg_outputs =
            grus_regalloc::run(&reg_wrapper, &grus_regalloc::simple_int_machine(4, 0))?;

        lirfun.apply_regalloc(&reg_wrapper, &reg_outputs);
        lirfun.lower_second();
        // lirfun.patch_returns();
        lirfun.patch_operations();

        buffer = lirfun.assemble();

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
