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
