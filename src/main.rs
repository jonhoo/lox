use clap::{Parser, Subcommand};
use codecrafters_interpreter as imp;
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
    Parse { filename: PathBuf },
    Run { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    let mut any_cc_err = false;
    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            for token in imp::Lexer::new(&file_contents) {
                let token = match token {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{e:?}");
                        if let Some(unrecognized) = e.downcast_ref::<imp::lex::SingleTokenError>() {
                            any_cc_err = true;
                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                unrecognized.line(),
                                unrecognized.token
                            );
                        } else if let Some(unterminated) =
                            e.downcast_ref::<imp::lex::StringTerminationError>()
                        {
                            any_cc_err = true;
                            eprintln!("[line {}] Error: Unterminated string.", unterminated.line(),);
                        }
                        continue;
                    }
                };
                println!("{token}");
            }
            println!("EOF  null");
        }
        Commands::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let parser = imp::Parser::new(&file_contents);
            println!("{}", parser.parse_expression().unwrap());
        }
        Commands::Run { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let parser = imp::Parser::new(&file_contents);
            println!("{}", parser.parse().unwrap());
        }
    }

    if any_cc_err {
        std::process::exit(65);
    }

    Ok(())
}
