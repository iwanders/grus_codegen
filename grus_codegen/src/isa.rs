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
        // let frame_size = 0;

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
