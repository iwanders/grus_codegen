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

pub fn test_files<P: AsRef<std::path::Path> + std::fmt::Debug>(files: &[P]) -> Result<()> {
    for f in files {
        // let p: std::path::PathBuf = f.into();
        let f = std::fs::read_to_string(&f).context(format!("failed to open {f:?}"))?;
        //let fun = cranelift_reader::parse_test(&f)?;
        //println!("fun: {fun:?}");
    }
    todo!()
}
