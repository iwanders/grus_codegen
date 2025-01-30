// https://bouvier.cc/2021/02/17/cranelift-codegen-primer/

type ResultError = Box<dyn std::error::Error>;
type ResultReturn = Result<(), ResultError>;

// https://github.com/bytecodealliance/wasmtime/issues/10118
fn attempt_two() -> ResultReturn {
    let args: Vec<String> = std::env::args().collect();

    use grus_codegen::X86Isa;

    let isa = X86Isa::new();

    println!("isa.triple(): {}", &isa.triple());
    // println!("isa: {}", &isa);

    let f = args.get(1).expect("expected one argument filename");
    println!("reading: {f:?}");
    let f = std::fs::read_to_string(&f)?;

    let mut fun = cranelift_reader::parse_functions(&f)?;
    let fun = fun.drain(..).next().unwrap();
    println!("fun: {fun:?}");

    Ok(())
}

fn main() -> ResultReturn {
    println!("Hello, world!");
    // attempt_one()?;
    attempt_two()?;
    Ok(())
}
