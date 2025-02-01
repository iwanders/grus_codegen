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

    let mut fun_trampolines: Vec<Box<dyn Fn() -> u64>> = vec![];
    for (i, (fun, _detail)) in test_file.functions.iter().enumerate() {
        let p = jit_module
            .get_fun(&funids[i])
            .context(format!("could not find function"))?;
        println!("p: {p:?}");

        // Okay, so we now have a pointer p, that contains our actual function.
        // next is setting up the registers... and performing the jump there.
        // Cranelift's make_trampoline looks complex, but what it really does is load values from
        // memory into registers according to the calling convention, then jumping to the callee.

        // For now, we can make this a bit simpler, since we don't take any arguments so far...
        // Probably this works?
        fun_trampolines.push(Box::new(move || {
            use std::arch::asm;
            let mut x: u64 = 0;
            unsafe {
                asm!(
                    "call {p}",
                    "ret",
                    // x = inout(reg) x,
                    p = in(reg) p,
                );
            }
            x
        }));
    }

    for (i, (f, detail)) in test_file.functions.iter().enumerate() {
        for c in detail.comments.iter() {
            if let Some(cmd) = cranelift_reader::parse_run_command(c.text, &f.signature)? {
                println!("cmd: {cmd:?}");
                if let cranelift_reader::RunCommand::Run(invocation, comparison, args) = cmd {
                    println!("invocation: {:?}, {:?}", invocation.func, invocation.args);
                    println!("comparison: {comparison:?}");
                    println!("args: {args:?}");
                    // lets goooo!
                    let trampo = &fun_trampolines[i];
                    let res = (*trampo)();
                }
            }
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
