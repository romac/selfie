use std::path::Path;

use ariadne::Source;
use thiserror::Error;

use selfie::ast::Program;
use selfie::lexer::Error as LexError;
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
    #[error("lexer error")]
    Lex(LexError),

    #[error("parse error")]
    Parse(ParseError),

    #[error("namer error")]
    Namer(Box<NamerError>),
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
    let tokens = selfie::lexer::lex(input).map_err(|e| vec![Error::Lex(e)])?;

    let module = selfie::parser::parse_module(tokens, path)
        .map_err(|errs| errs.into_iter().map(Error::Parse).collect::<Vec<_>>())?;

    let mut program = Program {
        modules: vec![module],
    };

    let cg = program.build_call_graph();

    for fn_decl in program.fns() {
        dbg!(cg.build_fn_info(fn_decl.sym));
    }

    let syms = selfie::namer::name_program(&mut program).map_err(|errs| {
        errs.into_iter()
            .map(|e| Error::Namer(Box::new(e)))
            .collect::<Vec<_>>()
    })?;

    println!("=== Namer - Symbols ===\n{syms:#?}\n");
    println!("=== Namer - Program ===\n{program:#?}\n");

    Ok(())
}

fn print_errors(path: &Path, input: &str, errors: Vec<Error>) {
    let id = path.display().to_string();

    for e in errors {
        let report = match e {
            Error::Lex(e) => report_lex_error(&e, &id),
            Error::Parse(e) => report_parse_error(&e, &id),
            Error::Namer(e) => report_namer_error(&e, &id),
        };

        let _ = report.eprint((id.clone(), Source::from(input)));
    }
}
