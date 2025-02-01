// use cranelift_reader::parse_functions;

/*
    Load the cliff file.
    Compile functions.
    Put in module.
    Craft super unsafe things.
    Call
    Check results.
*/

use anyhow::{Context, Result};
use cranelift_reader::TestFile;

use grus_module::{Linkage, ObjectModule};

pub fn process_test_file(test_file: &TestFile) -> Result<()> {
    // First, compile all functions.
    let isa = crate::X86Isa::new();

    let mut module = ObjectModule::new();
    let mut funids = vec![];
    for (fun, _detail) in test_file.functions.iter() {
        let res = isa.compile_function(&fun)?;
        let id = module.declare_named_function(Linkage::Export, &fun)?;
        module.define_function_bytes(id, 0, &res.buffer)?;
        funids.push(id);
    }
    let jit_module = module.jit()?;
    for (i, (fun, _detail)) in test_file.functions.iter().enumerate() {
        let p = jit_module.get_fun(&funids[i]);
        println!("p: {p:?}");
    }

    for (f, detail) in test_file.functions.iter() {
        for c in detail.comments.iter() {
            let z = cranelift_reader::parse_run_command(c.text, &f.signature)?;
            println!("z: {z:?}");
        }
    }
    Ok(())
}

pub fn test_files<P: AsRef<std::path::Path> + std::fmt::Debug>(files: &[P]) -> Result<()> {
    for f in files {
        let f = std::fs::read_to_string(&f).context(format!("failed to open {f:?}"))?;
        let test_file = cranelift_reader::parse_test(&f, Default::default())?;
        process_test_file(&test_file)?;
    }
    Ok(())
}
