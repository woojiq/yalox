use std::path::Path;

use twlox::runner;

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = concat!(env!("CARGO_PKG_NAME"), " v", env!("CARGO_PKG_VERSION"));

fn main() {
    let args = twlox::args::parse_cli_args().unwrap_or_else(|err| {
        eprintln!("{PKG_NAME}: {err}\n\n{}", help_message());
        std::process::exit(1);
    });

    if args.help {
        println!("{}", help_message());
        return;
    }
    if args.version {
        println!("{VERSION}");
        return;
    }

    let mut runner = runner::Runner::default();
    if let Some(src) = args.filename {
        runner.run_file(Path::new(&src));
    } else {
        runner.run_repl();
    }
}

fn help_message() -> String {
    format!(
        "\
{VERSION}
Lox programming language interpreter.

Usage:
    {PKG_NAME} <file-path>

Args:
    <file-path> Path to a file with the code.

Options:
    --help                  Prints help information
    --version               Prints version
    "
    )
}
