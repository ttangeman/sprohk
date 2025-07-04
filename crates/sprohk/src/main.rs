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
