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
use cranelift_codegen::data_value::DataValue;
use cranelift_reader::{Comparison, TestFile};

use grus_module::{Linkage, ObjectModule};
use log::{info, trace, warn};

pub fn process_test_file(test_file: &TestFile) -> Result<bool> {
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

        unsafe {
            let z = std::slice::from_raw_parts(p, 5);
            println!("z: {z:x?}");
        }

        // Okay, so we now have a pointer p, that contains our actual function.
        // next is setting up the registers... and performing the jump there.
        // Cranelift's make_trampoline looks complex, but what it really does is load values from
        // memory into registers according to the calling convention, then jumping to the callee.

        // For now, we can make this a bit simpler, since we don't take any arguments so far...
        // Probably this works?
        fun_trampolines.push(Box::new(move || {
            use std::arch::asm;
            let mut x: u64;
            unsafe {
                asm!(
                    "call {p};",
                    // "mov {x}, %eax;",
                    // x = inout(reg) x,
                    p = in(reg) p,
                    lateout("eax") x,
                );
            }
            x
        }));
    }

    let mut count_failures = 0;
    let mut count_success = 0;

    for (i, (f, detail)) in test_file.functions.iter().enumerate() {
        for c in detail.comments.iter() {
            if let Some(cmd) = cranelift_reader::parse_run_command(c.text, &f.signature)? {
                trace!("cmd: {cmd:?}");
                trace!(" sign: {:?}", f.signature);
                if let cranelift_reader::RunCommand::Run(invocation, comparison, args) = cmd {
                    trace!("invocation: {:?}, {:?}", invocation.func, invocation.args);
                    if !invocation.args.is_empty() {
                        todo!("Handle arguments in trampoline");
                    }
                    trace!("comparison: {comparison:?}");
                    trace!("args: {args:?}");
                    // lets goooo!
                    let trampo = &fun_trampolines[i];
                    let res = (*trampo)();
                    trace!("result: {res:?}");

                    match comparison {
                        Comparison::Equals => {
                            // args[1] + 3;
                            for (index, expected) in args.iter().enumerate() {
                                let return_type = f.signature.returns[index];
                                let returned =
                                    DataValue::from_integer(res as i128, return_type.value_type)?;
                                if returned == *expected {
                                    trace!("Comparison::Equals: {returned:?} == {expected:?}");
                                    count_success += 1;
                                } else {
                                    warn!(
                                        "FAILED Comparison::Equals: {returned:?} != {expected:?}"
                                    );
                                    count_failures += 1;
                                }
                            }
                        }
                        Comparison::NotEquals => todo!(),
                    }
                } else {
                    todo!()
                }
            }
        }
    }

    if count_failures != 0 {
        warn!("Failures: {count_failures}");
        info!("Success: {count_success}");
        return Ok(false);
    } else {
        info!("Success: {count_success}");
        return Ok(true);
    }
}

pub fn test_files<P: AsRef<std::path::Path> + std::fmt::Debug>(files: &[P]) -> Result<bool> {
    let mut all_passed = true;
    for f in files {
        let f = std::fs::read_to_string(&f).context(format!("failed to open {f:?}"))?;
        let test_file = cranelift_reader::parse_test(&f, Default::default())?;
        all_passed &= process_test_file(&test_file)?;
    }
    Ok(all_passed)
}
