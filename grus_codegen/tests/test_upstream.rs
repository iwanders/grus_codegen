use walkdir::WalkDir;

pub fn crawl_dir<P: Into<std::path::PathBuf>>(
    p: P,
) -> Result<Vec<std::path::PathBuf>, walkdir::Error> {
    let p: std::path::PathBuf = p.into();
    let mut v: Vec<std::path::PathBuf> = vec![];
    for entry in WalkDir::new(p)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let f_name = entry.file_name().to_string_lossy();
        if f_name.ends_with(".clif") {
            v.push(entry.path().into());
        }
    }
    Ok(v)
}

#[test]
fn test_upstream() -> Result<(), anyhow::Error> {
    let mut d = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("..");
    d.push("filetests");
    d.push("upstream");
    // d.push("filetests");

    let files = crawl_dir(d)?;
    println!("files: {files:#?}");
    let res = grus_codegen::clif_support::test_files(&files)?;
    assert!(res);
    println!("res: {res:#?}");

    Ok(())
}
