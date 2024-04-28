pub struct Args {
    pub filename: Option<String>,
}

pub fn parse_args() -> Args {
    let args = std::env::args().collect::<Vec<_>>();
    assert!(args.len() <= 2);

    Args { filename: if args.len() == 1 { None } else { Some(args[1].clone()) } }
}
