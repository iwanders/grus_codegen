// This is definitely butchered from
// https://github.com/bytecodealliance/wasmtime/blob/af3c0290680aca2fe158c2743f63fd3bedda83df/cranelift/object/src/backend.rs
use object::write::{Object, StandardSection, Symbol, SymbolId, SymbolSection};
use object::{SymbolFlags, SymbolKind, SymbolScope};
// Relocation, SectionId,
use cranelift_codegen::ir;
use cranelift_module::{FuncId, ModuleDeclarations, ModuleError, ModuleResult};

pub use cranelift_module::Linkage;
pub struct ObjectModule {
    object: Object<'static>,
    functions: std::collections::HashMap<FuncId, Option<(SymbolId, bool)>>,
    declarations: ModuleDeclarations,
}

fn translate_linkage(linkage: Linkage) -> (SymbolScope, bool) {
    let scope = match linkage {
        Linkage::Import => SymbolScope::Unknown,
        Linkage::Local => SymbolScope::Compilation,
        Linkage::Hidden => SymbolScope::Linkage,
        Linkage::Export | Linkage::Preemptible => SymbolScope::Dynamic,
    };
    // TODO: this matches rustc_codegen_cranelift, but may be wrong.
    let weak = linkage == Linkage::Preemptible;
    (scope, weak)
}

impl ObjectModule {
    pub fn new() -> Self {
        let binary_format = object::BinaryFormat::Elf;
        let architecture = object::Architecture::X86_64;
        let endian = object::Endianness::Little;
        Self {
            object: Object::new(binary_format, architecture, endian),
            functions: Default::default(),
            declarations: Default::default(),
        }
    }

    pub fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &ir::Signature,
    ) -> ModuleResult<FuncId> {
        let (id, linkage) = self
            .declarations
            .declare_function(name, linkage, signature)?;

        let (scope, weak) = translate_linkage(linkage);

        let symbol_id = self.object.add_symbol(Symbol {
            name: name.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope,
            weak,
            section: SymbolSection::Undefined,
            flags: SymbolFlags::None,
        });
        self.functions.insert(id, Some((symbol_id, false)));
        Ok(id)
    }

    pub fn define_function_bytes(
        &mut self,
        func_id: FuncId,
        // func: &ir::Function,
        alignment: u64,
        bytes: &[u8],
        // relocs: &[FinalizedMachReloc],
    ) -> ModuleResult<()> {
        let decl = self.declarations.get_function_decl(func_id);
        let decl_name = decl.linkage_name(func_id);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(decl_name.into_owned()));
        }

        let &mut (symbol, ref mut defined) = self
            .functions
            .get_mut(&func_id)
            .map(|z| z.as_mut())
            .flatten()
            .unwrap();
        if *defined {
            return Err(ModuleError::DuplicateDefinition(decl_name.into_owned()));
        }
        *defined = true;

        let function_alignment = 32;
        let symbol_alignment = 32;
        let align = alignment.max(function_alignment).max(symbol_alignment);

        let section = self.object.section_id(StandardSection::Text);
        let offset = self.object.add_symbol_data(symbol, section, bytes, align);
        let _ = offset;

        Ok(())
    }

    pub fn finish(self) -> Result<Vec<u8>, object::write::Error> {
        self.object.write()
    }
}
