use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::dominator_tree::DominatorTree;
use cranelift_codegen::ir::{Function, Type};
use cranelift_codegen::isa::{
    unwind::{UnwindInfo, UnwindInfoKind},
    FunctionAlignment, TargetIsa,
};
use cranelift_codegen::settings::{Flags, Value};
use cranelift_codegen::{CodegenResult, CompiledCode, TextSectionBuilder}; // CodegenError, Final

// This one is actually private...?
use cranelift_codegen::CompiledCodeStencil;

use target_lexicon::Triple;

pub struct X86Isa {
    triple: Triple,
    flags: Flags,
}

impl X86Isa {
    pub fn new(flags: Flags) -> Self {
        Self {
            triple: target_lexicon::triple!("x86_64"),
            flags,
        }
    }
}

impl std::fmt::Display for X86Isa {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(fmt, "X86Isa")
    }
}

impl TargetIsa for X86Isa {
    fn name(&self) -> &'static str {
        "X86Isa"
    }
    fn triple(&self) -> &Triple {
        &self.triple
    }
    fn flags(&self) -> &Flags {
        &self.flags
    }
    fn isa_flags(&self) -> Vec<Value> {
        vec![]
    }
    fn dynamic_vector_bytes(&self, dynamic_ty: Type) -> u32 {
        let _ = dynamic_ty;
        16
    }
    fn compile_function(
        &self,
        func: &Function,
        domtree: &DominatorTree,
        want_disasm: bool,
        ctrl_plane: &mut ControlPlane,
    ) -> CodegenResult<CompiledCodeStencil> {
        let _ = (func, domtree, want_disasm, ctrl_plane);
        todo!()
    }
    fn emit_unwind_info(
        &self,
        result: &CompiledCode,
        kind: UnwindInfoKind,
    ) -> CodegenResult<Option<UnwindInfo>> {
        let _ = (result, kind);
        Ok(None)
    }
    fn text_section_builder(
        &self,
        num_labeled_funcs: usize,
    ) -> Box<(dyn TextSectionBuilder + 'static)> {
        let _ = num_labeled_funcs;
        todo!()
    }

    fn function_alignment(&self) -> FunctionAlignment {
        // Is this in the object?
        FunctionAlignment {
            minimum: 1,
            preferred: 32,
        }
    }

    fn page_size_align_log2(&self) -> u8 {
        // Todo: figure out what this means!
        debug_assert_eq!(1 << 12, 0x1000);
        12
    }

    fn has_native_fma(&self) -> bool {
        false
    }
    fn has_x86_blendv_lowering(&self, ty: Type) -> bool {
        let _ = ty;
        false
    }
    fn has_x86_pshufb_lowering(&self) -> bool {
        false
    }
    fn has_x86_pmulhrsw_lowering(&self) -> bool {
        false
    }
    fn has_x86_pmaddubsw_lowering(&self) -> bool {
        false
    }
}
