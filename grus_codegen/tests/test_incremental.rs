#[test]
fn test_incremental() -> Result<(), anyhow::Error> {
    let mut d = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("..");
    d.push("filetests");

    let filenames = [
        "0001_constant_i64.clif",
        "0002_arithmetic_add_i64_i32_i16_i8.clif",
        "0003_more_instructions.clif",
        "0004_arithmetic_sub_i64_i32_i16_i8.clif",
        "0005_arithmetic_mul_i64_i32_i16_i8.clif",
    ];

    for f in filenames {
        let mut fpath = d.clone();
        fpath.push(f);
        let res = grus_codegen::clif_support::test_files(&[fpath])?;
        println!("res: {res:#?}");
        assert!(res);
    }

    Ok(())
}
