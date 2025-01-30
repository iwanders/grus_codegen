use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::dominator_tree::DominatorTree;
use cranelift_codegen::ir::{Function, Type};
use cranelift_codegen::isa::{
    unwind::{UnwindInfo, UnwindInfoKind},
    FunctionAlignment, TargetIsa,
};
use cranelift_codegen::settings::{Flags, Value};
use cranelift_codegen::{CodegenResult, CompiledCode, TextSectionBuilder}; // CodegenError, Final

// This one is actually private...... and it's a pretty complex struct, shelving this approach
// for now and instead relying on the module's declare function. We will keep the same interface
// roughly, but just not define the trait.
use cranelift_codegen::CompiledCodeStencil;

// More problems, the cranelift_object::ObjectBuilder also takes an isa, so we can't even call into
// define_function_bytes for a module and then rely on our own assembly.

// And CompiledCodeStencil is also ghastly complex to work with :<.

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
        let frame_size = 0;
        // let buffer = cranelift_codegen::MachBuffer::<cranelift_codegen::machineinst::Stencil>::new();
        let buffer = todo!();
        Ok(CompiledCodeStencil {
            buffer,
            // Size of stack frame, in bytes.
            frame_size,

            // Disassembly, if requested.
            vcode: Default::default(),

            // Debug info related.
            value_labels_ranges: Default::default(),
            sized_stackslot_offsets: Default::default(),
            dynamic_stackslot_offsets: Default::default(),

            // only generated if machine_code_cfg_info
            bb_starts: Default::default(),
            bb_edges: Default::default(),
        })
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
