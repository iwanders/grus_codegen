use clap::{Parser, Subcommand};
use grus_codegen::clif_support::RegisterMachine;

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

fn register_machine_parser(s: &str) -> Result<RegisterMachine, serde_json::Error> {
    serde_json::from_str::<RegisterMachine>(&format!("\"{}\"", s))
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

        /// The function index to use in this file.
        #[clap(long, short, default_value = "0")]
        index: usize,

        #[clap(long, default_value="Int4", value_parser=register_machine_parser)]
        register_machine: RegisterMachine,
    },
}

use anyhow::Result;

fn main() -> Result<()> {
    let cli = Cli::parse();
    // env_logger::init();
    use env_logger::Env;
    env_logger::Builder::from_env(Env::default().default_filter_or("debug")).init();

    match &cli.command {
        Commands::Test { files } => {
            grus_codegen::clif_support::test_files(&files)?;
        }
        Commands::RegAlloc {
            file,
            index,
            register_machine,
        } => {
            grus_codegen::clif_support::reg_alloc(&file, *index, *register_machine)?;
        }
    }
    Ok(())
}
