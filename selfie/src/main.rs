use std::path::Path;

use ariadne::Source;
use selfie_parser::ParseError;

use selfie::report::parse_error_to_report;

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

    let path = Path::new(&args[1]);
    let input = std::fs::read_to_string(path)?;
    let module = selfie::parser::parse_module(&input);

    match module {
        Ok(module) => {
            println!("{:#?}", module);
        }

        Err(errors) => {
            print_errors(path, &input, errors);
            std::process::exit(1);
        }
    }

    Ok(())
}

fn print_errors(path: &Path, input: &str, errors: Vec<ParseError>) {
    let id = path.display().to_string();

    for e in errors {
        let report = parse_error_to_report(&e, id.clone());
        let _ = report.eprint((id.clone(), Source::from(input)));
    }
}
