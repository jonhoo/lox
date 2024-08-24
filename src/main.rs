use clap::{Parser, Subcommand};
use codecrafters_interpreter::*;
use miette::{IntoDiagnostic, WrapErr};
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = token?;
                println!("{token}");
            }
            println!("EOF  null");
        }
    }

    Ok(())
}
