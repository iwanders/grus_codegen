use object::write::{Object, StandardSection, Symbol, SymbolId, SymbolSection};
use object::{SymbolFlags, SymbolKind, SymbolScope};
// Relocation, SectionId,
use cranelift_codegen::ir;
use cranelift_module::{FuncId, ModuleDeclarations, ModuleError, ModuleResult};
use log::*;

pub use cranelift_module::Linkage;

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

use std::sync::Arc;
pub struct JitModule {
    module: ObjectModule,
    memory: Arc<memmap::Mmap>,
}

impl JitModule {
    pub fn get_fun(&self, func_id: &FuncId) -> Option<*const u8> {
        let decl = self.module.declarations.get_function_decl(*func_id);

        let name = decl.name.as_ref()?;
        trace!("name: {name:?}");

        use object::{Object, ObjectSection, ObjectSymbol};
        let elf =
            if let object::read::File::Elf64(elf) = object::File::parse(&**self.memory).ok()? {
                elf
            } else {
                return None;
            };

        for s in elf.symbols() {
            if s.name() == Ok(name) {
                trace!("s: {s:?}");
                trace!("address: {:?}", s.address());
                trace!("size: {:?}", s.size());
                trace!("section: {:?}", s.section());

                let section_id = s.section_index()?;
                trace!("section_id: {:?}", section_id);
                let section = elf.section_by_index(section_id).ok()?;
                let section_data = section.data().ok()?;
                trace!("section_data: {:?}", section_data);
                let address = s.address() as usize;
                return Some(&section_data[address] as *const u8);
            }
        }

        panic!("could not find the right symbol")
    }

    pub fn write<P: Into<std::path::PathBuf>>(&self, p: P) -> Result<(), anyhow::Error> {
        let p: std::path::PathBuf = p.into();
        // todo!()
        let bytes = self.module.object.write()?;
        std::fs::write(p, bytes)?;
        Ok(())
    }
}

// This is definitely butchered from
// https://github.com/bytecodealliance/wasmtime/blob/af3c0290680aca2fe158c2743f63fd3bedda83df/cranelift/object/src/backend.rs
pub struct ObjectModule {
    object: Object<'static>,
    functions: std::collections::HashMap<FuncId, Option<(SymbolId, bool)>>,
    declarations: ModuleDeclarations,
}

impl Default for ObjectModule {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn declare_named_function(
        &mut self,
        linkage: Linkage,
        fun: &ir::Function,
    ) -> ModuleResult<FuncId> {
        let name = format!("{}", fun.name);
        self.declare_function(&name, linkage, &fun.signature)
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
            .and_then(|z| z.as_mut())
            .unwrap();
        if *defined {
            return Err(ModuleError::DuplicateDefinition(decl_name.into_owned()));
        }
        *defined = true;

        let function_alignment = 0x1000;
        let symbol_alignment = 0x1000;
        let align = alignment.max(function_alignment).max(symbol_alignment);

        let section = self.object.section_id(StandardSection::Text);
        let offset = self.object.add_symbol_data(symbol, section, bytes, align);
        let _ = offset;

        Ok(())
    }

    pub fn finish(self) -> Result<Vec<u8>, object::write::Error> {
        self.object.write()
    }

    pub fn jit(self) -> Result<JitModule, anyhow::Error> {
        use std::io::Write;
        let data = self.object.write().expect("failed to write file");
        let mut memory = memmap::MmapMut::map_anon(data.len())?;
        (&mut memory[..]).write_all(&data)?;
        let exec_area = memory.make_exec()?;
        let pointered = Arc::new(exec_area);
        Ok(JitModule {
            module: self,
            memory: pointered,
        })
    }
}
