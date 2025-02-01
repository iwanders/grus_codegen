use cranelift_codegen::ir::Function;
use cranelift_codegen::CodegenResult;

use target_lexicon::Triple;

use log::*;

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

    pub fn compile_function(&self, func: &Function) -> CodegenResult<CompiledCode> {
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
        */

        let dfg = &stencil.dfg;
        for b in stencil.layout.blocks() {
            debug!("b: {b:?}");
            let block_data = &dfg.blocks[b];
            debug!(
                "block_data params: {:?}",
                block_data.params(&dfg.value_lists)
            );
        }

        const NOP: u8 = 0x90;
        const RETN: u8 = 0xc3;
        const INT3: u8 = 0xcc;
        // let buffer = vec![0xB8, 0x46, 0x00, 0x00, 0x00, NOP, NOP, INT3, RETN];
        // 0xB8, 0x46, 0x00, 0x00, 0x00 is writing 0x46 to EAX
        let buffer = vec![0xB8, 0x46, 0x00, 0x00, 0x00, NOP, NOP, RETN];
        Ok(CompiledCode {
            buffer,
            // Size of stack frame, in bytes.
            // frame_size,
        })
    }
}
