#![allow(clippy::needless_range_loop)]
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

use crate::{RegisterAllocator, RegisterMachine};

pub struct TestSettings {
    pub register_allocator: RegisterAllocator,
    pub register_machine: RegisterMachine,
    pub fun_index: Option<usize>,
    pub write_svg: Option<std::path::PathBuf>,
    pub register_trap: bool,
}
impl TestSettings {
    pub fn to_compile_settings(&self) -> crate::isa::CompileSettings {
        crate::isa::CompileSettings {
            register_allocator: self.register_allocator,
            register_machine: self.register_machine,
            write_svg: self.write_svg.clone(),
        }
    }
}

pub fn process_test_file(test_file: &TestFile, settings: &TestSettings) -> Result<bool> {
    // First, compile all functions.
    let isa = crate::X86Isa::new();

    let mut module = ObjectModule::new();
    let mut funids = vec![];
    for (fun, _detail) in test_file.functions.iter() {
        let res = isa.compile_function(fun, &settings.to_compile_settings())?;
        let id = module.declare_named_function(Linkage::Export, fun)?;
        module.define_function_bytes(id, 0, &res.buffer)?;
        funids.push(id);
    }
    let jit_module = module.jit()?;
    jit_module.write("/tmp/foo.o")?;

    type TrampolineSignature = dyn Fn(&Vec<DataValue>) -> u64;
    let mut fun_trampolines: Vec<Box<TrampolineSignature>> = vec![];
    for (i, (_fun, _detail)) in test_file.functions.iter().enumerate() {
        let p = jit_module
            .get_fun(&funids[i])
            .context("could not find function")?;
        // println!("p: {p:?}");

        // unsafe {
        // let z = std::slice::from_raw_parts(p, 5);
        // println!("z: {z:x?}");
        // }

        // Okay, so we now have a pointer p, that contains our actual function.
        // next is setting up the registers... and performing the jump there.
        // Cranelift's make_trampoline looks complex, but what it really does is load values from
        // memory into registers according to the calling convention, then jumping to the callee.

        // For now, we can make this a bit simpler, since we don't take any arguments so far...
        // Probably this works?
        fun_trampolines.push(Box::new(move |args: &Vec<DataValue>| {
            // RDI, RSI, RDX, RCX, R8, R9
            if args.len() > 6 {
                todo!("handle functions with more than 6 arguments");
            }

            let mut regs = [0u64; 6];
            for i in 0..5 {
                regs[i] = args
                    .get(i)
                    .map(|v| {
                        let mut b = [0u8; 8];
                        v.write_to_slice_le(&mut b);
                        u64::from_le_bytes(b)
                    })
                    .unwrap_or(0);
            }
            warn!("setting up regs: {regs:?}");

            use std::arch::asm;
            let mut x: u64;
            unsafe {
                // This is AT&T syntax, source before destination.
                asm!(
                    // "nop",
                    "call {p};",
                    // "mov {x}, %eax;",
                    // x = inout(reg) x,
                    p = in(reg) p,
                    in("rdi") regs[0],
                    in("rsi") regs[1],
                    in("rdx") regs[2],
                    in("rcx") regs[3],
                    in("r8") regs[4],
                    in("r9") regs[5],
                    lateout("rax") x,
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
                    // if !invocation.args.is_empty() {
                    // todo!("Handle arguments in trampoline");
                    // }
                    trace!("comparison: {comparison:?}");
                    trace!("args: {args:?}");
                    // lets goooo!
                    let trampo = &fun_trampolines[i];
                    trace!("Invoking the trampoline... 🤞");
                    let res = (*trampo)(&invocation.args);
                    trace!("result: {res:?}");

                    match comparison {
                        Comparison::Equals => {
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
        Ok(false)
    } else {
        info!("Success: {count_success}");
        Ok(true)
    }
}

pub fn test_files<P: AsRef<std::path::Path> + std::fmt::Debug>(
    files: &[P],
    settings: &TestSettings,
) -> Result<bool> {
    if settings.register_trap {
        crate::trap::setup_int3();
    }

    let mut all_passed = true;
    for f in files {
        let f = std::fs::read_to_string(f).context(format!("failed to open {f:?}"))?;
        let test_file = cranelift_reader::parse_test(&f, Default::default())?;
        all_passed &= process_test_file(&test_file, settings)?;
    }
    Ok(all_passed)
}

pub fn reg_alloc<P: AsRef<std::path::Path> + std::fmt::Debug>(
    file: &P,
    settings: &TestSettings,
) -> Result<()> {
    let f = std::fs::read_to_string(file).context(format!("failed to open {file:?}"))?;
    let test_file = cranelift_reader::parse_test(&f, Default::default())?;

    let fun_index = settings.fun_index.unwrap_or(0);
    let regmachine = settings.register_machine;

    let function = test_file
        .functions
        .get(fun_index)
        .context(format!("fun index {fun_index} out of bounds"))?;
    let func = &function.0;

    let mut lirfun = crate::lir::Function::from_ir(&func);
    lirfun.lirify();
    lirfun.lower_first();
    let reg_wrapper = lirfun.reg_wrapper();
    println!("{lirfun:#?}");
    lirfun.dump();
    let env = regmachine.to_env();

    let reg_rows = crate::lir::write_regfunction(&reg_wrapper, &reg_wrapper);
    for r in reg_rows {
        println!("{r}");
    }

    /*
    if let Some(regalloc_serialize_path) = write_regalloc_serialize {
        let serfun = regalloc2::serialize::SerializableFunction::new(&reg_wrapper, env.clone());
        use std::io::Write;
        let file = std::fs::File::create(regalloc_serialize_path)?;
        let mut writer = std::io::BufWriter::new(file);
        serde_json::to_writer(&mut writer, &serfun)?;
        writer.flush()?;
    }
    */

    let reg_outputs = match settings.register_allocator {
        RegisterAllocator::Winged => grus_regalloc::run(&reg_wrapper, &env)?,
        RegisterAllocator::Regalloc2Ion | RegisterAllocator::Regalloc2Fastalloc => {
            let options = regalloc2::RegallocOptions {
                verbose_log: false,
                validate_ssa: true,
                algorithm: settings
                    .register_allocator
                    .to_regalloc2_algorithm()
                    .unwrap(),
            };
            regalloc2::run(&reg_wrapper, &env, &options)?
        }
    };
    println!("reg_outputs: {reg_outputs:#?}");
    if let Some(svg_output_path) = &settings.write_svg {
        let options = Default::default();
        let document = grus_regalloc::svg::register_document(
            &reg_wrapper,
            &reg_outputs,
            &env,
            &options,
            &reg_wrapper,
        );
        grus_regalloc::svg::svg::save(svg_output_path, &document)?;
    }

    Ok(())
}
