use clap::Parser;

use sprohk_lexer::Tokenizer;

#[derive(Debug, Clone, clap::Subcommand)]
enum Command {
    Run { source_file: String },
}

#[derive(Debug, clap::Parser)]
#[command(name = "sprohk", version = "0.1")]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, PartialEq)]
enum ExitCode {
    Success,
    Error,
}

fn run(source_file: &str) -> ExitCode {
    match std::fs::read_to_string(source_file) {
        Ok(source) => {
            let mut tokenizer = Tokenizer::new(&source);
            let mut tokens = Vec::new();

            println!("{:?}", tokenizer.next());

            while let Some(token) = tokenizer.next() {
                tokens.push(token);
            }

            println!("Parsed {} Tokens:", tokens.len());
            for token in tokens {
                println!("{:?}", token);
            }

            return ExitCode::Success;
        }
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => {
                eprintln!("Error: Source file '{}' not found.", source_file);
            }
            std::io::ErrorKind::IsADirectory => {
                eprintln!("Error: '{}' is a directory, not a file.", source_file);
            }
            _ => {
                eprintln!("Error reading source file '{}': {}", source_file, e);
            }
        },
    }

    ExitCode::Error
}

fn main() {
    let args = Args::parse();

    let exit_code = match args.command {
        Command::Run { source_file } => run(&source_file),
    };

    std::process::exit(match exit_code {
        ExitCode::Success => 0,
        ExitCode::Error => 1,
    });
}

#[cfg(test)]
mod tests {
    use std::env;
    use super::*;

    // TODO: See if there is an easier way than changing the current directory
    fn change_dir_to_test() {
        let crate_dir = env!("CARGO_MANIFEST_DIR");
        let test_dir = format!("{}/tests", crate_dir);

        println!("Changing directory to: {}", test_dir);

        env::set_current_dir(&test_dir).expect("Failed to change directory to test directory");
    }

    #[test]
    fn text_example_00() {
        change_dir_to_test();

        let exit_code = run("00_idents-and-lits.spk");
        assert_eq!(
            exit_code,
            ExitCode::Success,
            "Expected success for test 00_idents-and-lits.spk"
        );
    }
}
