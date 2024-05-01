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
        let mut file = File::open(path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        match self.run(&content) {
            Ok(_) => (),
            Err(e) => e.print_to_stderr(),
        }
    }

    pub fn run_repl(&mut self) {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut line = String::new();
        loop {
            print!("> ");
            stdout.flush().unwrap();
            line.clear();

            stdin.read_line(&mut line).unwrap();
            if line.is_empty() {
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

#[cfg(test)]
mod test {
    use super::Runner;
    use crate::interpreter::value::Value;

    // TODO: Move to integration tests.
    #[test]
    fn condition() {
        let src = "var a = 0; if (2/1 > (3-4)*1) {a = 1;} else {a = 2;}";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("a"), Some(Value::Num(1.0).to_rc()));
    }

    #[test]
    fn for_loop() {
        let src = "var a = 0; var n = 5; for (var i=0; i<=n; i=i+1) {a = a + i + 0.1;}";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("a"), Some(Value::Num(15.6).to_rc()));
    }

    #[test]
    fn function() {
        let src = "fun mul(a, b) {var c = a + b; return c * c;} var res = mul(1, 1);";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("res"), Some(Value::Num(4.0).to_rc()));
    }

    #[test]
    fn function_global_var() {
        let src = "var a = 0; fun mul(a) {a = a + 1;} mul(a); mul(a);";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("a"), Some(Value::Num(2.0).to_rc()));
    }

    #[test]
    fn scoping() {
        let src = "\
var a = \"global\"; var b;
{ fun changeA() { b = a; } changeA(); var a = \"block\"; changeA(); }
print b;
";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("b"), Some(Value::Str("global".to_string()).to_rc()));
    }

    #[test]
    fn class() {
        let src = "\
var v = 0;
class Bacon {
    eat(v) {
        v = v + 1;
    }
}
var v2 = 0;
Bacon().eat(v); Bacon().eat(v2); Bacon().eat(v);
";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("v"), Some(Value::Num(2.0).to_rc()));
        assert_eq!(env.get("v2"), Some(Value::Num(1.0).to_rc()));
    }

    #[test]
    fn self_assignment() {
        let src = "\
var a = 1;
var a = a;
var b = 2;
b = b;
";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("a"), Some(Value::Num(1.0).to_rc()));
        assert_eq!(env.get("b"), Some(Value::Num(2.0).to_rc()));
    }

    #[test]
    fn class_with_this_keyword() {
        let src = "\
class Test {
    init(v) {
        this.v = 0;
        return;
    }
    add() {
        this.v = this.v + 1;
    }
}
var test = Test(0);
test = test.init(0);
test.add(); test.add(); test.add();
var res = test.v;
";
        let mut runner = Runner::new();
        runner.run(src).unwrap();
        let env = runner.interpreter.env();
        assert_eq!(env.get("res"), Some(Value::Num(3.0).to_rc()));
    }
}
