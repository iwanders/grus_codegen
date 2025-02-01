use cranelift_codegen::ir::{Function, InstructionData};
use cranelift_codegen::CodegenResult;

use log::*;
use target_lexicon::Triple;

/*

    253665-sdm-vol-1-dec-24.pdf:
        3.2.1: 64 bit mode execution; 16 general purpose registers.
        3.4.1, p73;
            General purpose registers are able to work with 32 bit and 64 bit operands.

            p75;
             8 bit           AL,  BL,  CL,  DL,  SIL, DIL, BPL, and SPL, R8B-R15B
            16 bit           AX,  BX,  CX,  DX,  SI,  DI,  BP, and  SP, R8W-R15W
            32 bit operand: EAX, EBX, ECX, EDX, ESI, EDI, EBP, and ESP, R8D-R15D
            64 bit operand: RAX, RBX, RCX, RDX, RSI, RDI, RBP, and RSP, R8 - R15

            p74;
            EAX - Accumulator for operands and results data.
            EBX - Pointer to data in the DS segment.
            ECX - Counter for string and loop operations.
            EDX - I/O pointer.

            ESI - Pointer to data in the segment pointed to by the DS register; source pointer for string operations.
            EDI - Pointer to data (or destination) in the segment pointed to by the ES register;
                  destination pointer for string operations.
            ESP - Stack pointer (in the SS segment).
            EBP - Pointer to data on the stack (in the SS segment).

            64 bit registers; quadword
            32 bit registers; doubleword
            16 bit registers: word registers
             8 bit register: byte registers.
            oh, see also 4.1, p87

            EFLAGS is a special flag register that affects 'things'... see p78

            3.5, p80 describes instruction pointer.

            3.7, p82 describes operand addressing.

            3.7.5, p84 has displacement / offset specification information.

            4.2, up to p92 contains integer & float sizes and definitions.

            4.7, p 95... something called a binary-coded decimal integer.

            5.1, start of general purpose instructions.

*/

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

        https://docs.rs/cranelift-codegen/0.116.1/cranelift_codegen/ir/instructions/enum.InstructionData.html
        */

        let dfg = &stencil.dfg;
        let layout = &stencil.layout;
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
                match instdata {
                    InstructionData::UnaryImm { opcode, imm } => {
                        // How do we know where this goes..
                        // Line is `v1 = iconst.i8 0` in the clif... how do we get the Value of v1?
                    }
                    _ => todo!(
                        "unimplemented: {:?} in {:?}, of {:?}",
                        instdata,
                        b,
                        func.name
                    ),
                }
            }
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
