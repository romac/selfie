fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("Usage: selfie <file>");
        std::process::exit(1);
    }

    let input = std::fs::read_to_string(&args[1])?;
    let module = selfie::parser::parse(&input)?;

    println!("{module:#?}");

    Ok(())
}
