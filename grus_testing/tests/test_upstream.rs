#[test]
fn test_upstream() -> Result<(), anyhow::Error> {
    let mut d = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("upstream");
    d.push("filetests");

    let files = grus_testing::crawl_dir(d)?;
    println!("files: {files:#?}");
    let res = grus_codegen::clif_support::test_files(&files)?;
    assert!(res);
    println!("res: {res:#?}");

    Ok(())
}
