use anyhow::Context;

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

    let test_settings = grus_codegen::clif_support::TestSettings {
        register_allocator: grus_codegen::RegisterAllocator::Regalloc2Ion,
        register_machine: grus_codegen::RegisterMachine::Int4,
        fun_index: None,
        write_svg: None,
        register_trap: false,
    };

    for f in filenames {
        let mut fpath = d.clone();
        fpath.push(f);
        let res = grus_codegen::clif_support::test_files(&[fpath.clone()], &test_settings)
            .with_context(|| format!("testing file {fpath:?}"))?;
        if res {
            println!("Succeeded {fpath:?}");
        } else {
            println!("Failed {fpath:?}");
        }
        assert!(res);
    }

    Ok(())
}
