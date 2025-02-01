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

pub fn process_test_file(test_file: &TestFile) -> Result<()> {
    // First, compile all functions.

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
