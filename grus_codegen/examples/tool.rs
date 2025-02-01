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
}

use anyhow::Result;

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Test { files } => {
            grus_codegen::clif_support::test_files(&files)?;
        }
    }
    Ok(())
}
