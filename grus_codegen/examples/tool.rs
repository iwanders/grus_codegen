use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
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
        Commands::RegAlloc { file, index } => {
            grus_codegen::clif_support::reg_alloc(&file, *index)?;
        }
    }
    Ok(())
}
