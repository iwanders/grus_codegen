use clap::{Parser, Subcommand};
use grus_codegen::{RegisterAllocator, RegisterMachine};

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// The function index to use in this file.
    #[clap(long, short, default_value = "0")]
    fun_index: Option<usize>,

    #[clap(long, default_value="Int4", value_parser=register_machine_parser)]
    register_machine: RegisterMachine,

    #[clap(long)]
    write_svg: Option<std::path::PathBuf>,

    #[clap(long)]
    write_regalloc_serialize: Option<std::path::PathBuf>,

    #[clap(long, value_parser=register_allocator_parser, default_value="Regalloc2Ion")]
    allocator: RegisterAllocator,
}

fn register_machine_parser(s: &str) -> Result<RegisterMachine, serde_json::Error> {
    serde_json::from_str::<RegisterMachine>(&format!("\"{}\"", s))
}

fn register_allocator_parser(s: &str) -> Result<RegisterAllocator, serde_json::Error> {
    serde_json::from_str::<RegisterAllocator>(&format!("\"{}\"", s))
}

#[derive(Subcommand)]
enum Commands {
    /// Run a clif file test.
    Test {
        /// Files to run.
        files: Vec<std::path::PathBuf>,
    },
    /// Run a clif file through the register allocation.
    RegAlloc {
        /// File to run register allocation on.
        file: std::path::PathBuf,
    },
}

use anyhow::Result;

fn main() -> Result<()> {
    let cli = Cli::parse();
    // env_logger::init();
    use env_logger::Env;
    env_logger::Builder::from_env(Env::default().default_filter_or("debug")).init();

    let test_settings = grus_codegen::clif_support::TestSettings {
        register_allocator: cli.allocator,
        register_machine: cli.register_machine,
        fun_index: cli.fun_index,
        write_svg: cli.write_svg,
    };

    match &cli.command {
        Commands::Test { files } => {
            grus_codegen::clif_support::test_files(&files, &test_settings)?;
        }
        Commands::RegAlloc { file } => {
            grus_codegen::clif_support::reg_alloc(&file, &test_settings)?;
        }
    }
    Ok(())
}
