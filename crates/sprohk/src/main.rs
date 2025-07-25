use bumpalo::Bump;
use clap::Parser;
use sprohk_parser::parse_ast;
use sprohk_core::SourceFile;

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

fn run(source_file_path: String) -> ExitCode {
    let source_file = {
        match SourceFile::new(source_file_path) {
            Ok(source_file) => source_file,
            Err(err) => {
                eprintln!("Error reading source file: {}", err.to_string());
                return ExitCode::Error;
            }
        }
    };

    let sources = vec![source_file];
    let arena = Bump::with_capacity(1 * 1024 * 1024); // 1 MB arena for AST allocation
    let ast = parse_ast(&arena, sources);

    match ast {
        Ok(_ast) => {
            println!("Parsed AST successfully");
        }
        Err(e) => {
            eprintln!("Failed to parse source: {:?}", e);
            return ExitCode::Error;
        }
    }

    ExitCode::Success
}

fn main() {
    let args = Args::parse();

    let exit_code = match args.command {
        Command::Run { source_file } => run(source_file),
    };

    std::process::exit(match exit_code {
        ExitCode::Success => 0,
        ExitCode::Error => 1,
    });
}
