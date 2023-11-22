use std::path::Path;

use ariadne::Source;
use selfie_ast::Program;
use selfie_namer::Namer;
use thiserror::Error;

use selfie::namer::Error as NamerError;
use selfie::parser::Error as ParseError;

use selfie::report::*;

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("parse error")]
    Parse(#[source] ParseError),

    #[error("namer error")]
    Namer(#[source] NamerError),
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("Usage: selfie <file>");
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let input = std::fs::read_to_string(path)?;

    let result = pipeline(&input, path);

    if let Err(errors) = result {
        print_errors(path, &input, errors);
        std::process::exit(1);
    }

    Ok(())
}

fn pipeline(input: &str, path: &Path) -> Result<(), Vec<Error>> {
    let module = selfie::parser::parse_module(input, path)
        .map_err(|errs| errs.into_iter().map(Error::Parse).collect::<Vec<_>>())?;

    let mut program = Program {
        modules: vec![module],
    };

    let namer = Namer::new();
    let syms = namer
        .name_program(&mut program)
        .map_err(|errs| errs.into_iter().map(Error::Namer).collect::<Vec<_>>())?;

    dbg!(syms);
    dbg!(program);

    Ok(())
}

fn print_errors(path: &Path, input: &str, errors: Vec<Error>) {
    let id = path.display().to_string();

    for e in errors {
        match e {
            Error::Parse(e) => {
                let report = parse_error_to_report(&e, id.clone());
                let _ = report.eprint((id.clone(), Source::from(input)));
            }

            Error::Namer(e) => {
                let report = namer_error_to_report(&e, id.clone());
                let _ = report.eprint((id.clone(), Source::from(input)));
            }
        }
    }
}
