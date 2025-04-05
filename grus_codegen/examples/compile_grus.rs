// https://bouvier.cc/2021/02/17/cranelift-codegen-primer/

type ResultError = Box<dyn std::error::Error>;
type ResultReturn = Result<(), ResultError>;
use grus_module::{Linkage, ObjectModule};

// https://github.com/bytecodealliance/wasmtime/issues/10118
fn attempt_two() -> ResultReturn {
    let args: Vec<String> = std::env::args().collect();

    use grus_codegen::X86Isa;

    let isa = X86Isa::new();

    let test_settings = grus_codegen::clif_support::TestSettings {
        register_allocator: grus_codegen::RegisterAllocator::Winged,
        register_machine: grus_codegen::RegisterMachine::Int4,
        fun_index: None,
        write_svg: None,
        register_trap: false,
    };

    println!("isa.triple(): {}", &isa.triple());
    // println!("isa: {}", &isa);

    let f = args.get(1).expect("expected one argument filename");
    println!("reading: {f:?}");
    let f = std::fs::read_to_string(&f)?;

    let mut fun = cranelift_reader::parse_functions(&f)?;
    let fun = fun.drain(..).next().unwrap();
    println!("fun: {fun:?}");

    let res = isa.compile_function(&fun, &test_settings.to_compile_settings())?;

    let mut module = ObjectModule::new();
    let id = module.declare_function("foo", Linkage::Export, &fun.signature)?;
    module.define_function_bytes(id, 0, &res.buffer)?;

    let object_data = module.finish()?;

    let write_to_disk = true;
    if write_to_disk {
        // Looks legit and is modified whenever the clif is modified.
        std::fs::write("/tmp/average.o", object_data)?;
    }

    Ok(())
}

fn main() -> ResultReturn {
    println!("Hello, world!");
    // attempt_one()?;
    attempt_two()?;
    Ok(())
}
