use std::{
    fs::File,
    io::{self, Read, Write},
    path::Path,
};

use crate::{interpreter, parser, resolver, scanner};

#[derive(Debug, Clone)]
pub enum Error {
    Scanner(Vec<scanner::Error>),
    Parser(Vec<parser::Error>),
    Resolver(Vec<resolver::Error>),
    Interpreter(Vec<interpreter::Error>),
}

impl Error {
    pub fn print_to_stderr(self) {
        let strs: Vec<String> = self.into();
        eprintln!("Errors:");
        for (idx, err) in strs.into_iter().enumerate() {
            eprintln!("{}: {}", idx + 1, err);
        }
    }
}

#[allow(clippy::from_over_into)]
impl Into<Vec<String>> for Error {
    fn into(self) -> Vec<String> {
        match self {
            Error::Scanner(v) => v.into_iter().map(|e| e.to_string()).collect(),
            Error::Parser(v) => v.into_iter().map(|e| e.to_string()).collect(),
            Error::Interpreter(v) => v.into_iter().map(|e| e.to_string()).collect(),
            Error::Resolver(v) => v.into_iter().map(|e| e.to_string()).collect(),
        }
    }
}

pub struct Runner {
    interpreter: interpreter::Interpreter,
}

impl Runner {
    pub fn new() -> Self {
        Self { interpreter: interpreter::Interpreter::new() }
    }

    pub fn run_file(&mut self, path: &Path) {
        let mut file = File::open(path).unwrap_or_else(|err| {
            eprintln!("Error: cannot open {path:?} file.\nReason: {err}");
            std::process::exit(1);
        });
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        match self.run(&content) {
            Ok(_) => (),
            Err(e) => e.print_to_stderr(),
        }
    }

    pub fn run_repl(&mut self) {
        const REPL_EXIT: &str = "exit";
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut line = String::new();
        println!("Lox REPL <3\nType Ctrl-D or \"{REPL_EXIT}\" to quit!");
        loop {
            print!("> ");
            stdout.flush().unwrap();
            line.clear();

            // TODO: Support basic motions (C-p, C-n, left, right, etc).
            stdin.read_line(&mut line).unwrap();
            if line.is_empty() || line.trim() == REPL_EXIT {
                break;
            }
            match self.run(&line) {
                Ok(_) => (),
                Err(e) => e.print_to_stderr(),
            }
        }
    }

    pub fn run(&mut self, src: &str) -> Result<(), Error> {
        let mut scanner = scanner::Scanner::new();
        let tokens = scanner.scan(src).map_err(Error::Scanner)?;

        let mut parser = parser::Parser::new(tokens);
        let statements = parser.parse().map_err(Error::Parser)?;

        let mut callback = |entry| self.interpreter.resolve(entry);
        let resolver = resolver::Resolver::new(&mut callback);
        resolver.resolve(&statements).map_err(Error::Resolver)?;

        self.interpreter.interpret(&statements).map_err(|e| Error::Interpreter(vec![e]))
    }
}

impl Default for Runner {
    fn default() -> Self {
        Self::new()
    }
}
