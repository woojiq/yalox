#[derive(Default)]
pub struct Args {
    pub filename: Option<String>,
    pub help: bool,
    pub version: bool,
}

pub fn parse_cli_args() -> Result<Args, lexopt::Error> {
    use lexopt::prelude::*;

    let mut args = Args::default();

    let mut parser = lexopt::Parser::from_env();
    while let Some(arg) = parser.next()? {
        match arg {
            Long("help") => args.help = true,
            Long("version") => args.version = true,
            Value(filename) => args.filename = Some(filename.parse()?),
            _ => return Err(arg.unexpected()),
        }
    }

    Ok(args)
}

#[derive(thiserror::Error)]
#[derive(Debug, Clone)]
pub enum Error {
    #[error("expected only number of file to run")]
    MaxNumOfArgs,
}
