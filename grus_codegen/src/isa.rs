use cranelift_codegen::ir::Function;
use cranelift_codegen::CodegenResult;

use target_lexicon::Triple;

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
    pub frame_size: usize,
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
        let frame_size = 0;
        // let buffer = cranelift_codegen::MachBuffer::<cranelift_codegen::machineinst::Stencil>::new();
        const NOP: u8 = 0x90;
        const RETN: u8 = 0xc3;
        let buffer = vec![NOP, NOP, NOP, RETN];
        Ok(CompiledCode {
            buffer,
            // Size of stack frame, in bytes.
            frame_size,
        })
    }
}
