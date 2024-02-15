use std::path::Path;

use twlox::{args, runner};

fn main() {
    let args = args::parse_args();
    let mut runner = runner::Runner::default();
    if let Some(src) = args.filename {
        runner.run_file(Path::new(&src));
    } else {
        runner.run_repl();
    }
}
