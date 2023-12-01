use std::fmt;
use std::path::Path;
use std::sync::Mutex;

use ariadne::Source;
use clap::Parser;
use thiserror::Error;

use selfie::ast::Program;
use selfie::cli::{Cli, DebugSection, DebugSections};
use selfie::lexer::Error as LexError;
use selfie::namer::Error as NameError;
use selfie::parser::Error as ParseError;
use selfie::report::*;
use selfie::typer::Error as TypeError;

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

    #[error("name error")]
    Name(Box<NameError>),

    #[error("type error")]
    Type(Box<TypeError>),
}

static DEBUG_SECTIONS: Mutex<DebugSections> = Mutex::new(DebugSections::new());

fn debug(section: DebugSection, args: fmt::Arguments) {
    let sections = DEBUG_SECTIONS.lock().unwrap();

    if sections.contains(section) {
        eprintln!("{args}");
    }
}

macro_rules! debug {
    ($section:tt, $($arg:tt)*) => {
        debug(DebugSection::$section, format_args!($($arg)*));
    };
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let Cli { file, debug } = Cli::parse();

    *DEBUG_SECTIONS.lock().unwrap() = debug;

    let input = std::fs::read_to_string(&file)?;

    let result = pipeline(&input, &file);

    if let Err(errors) = result {
        print_errors(&file, &input, errors);
        std::process::exit(1);
    }

    Ok(())
}

fn pipeline(input: &str, path: &Path) -> Result<(), Vec<Error>> {
    debug!(Lex, "=== Lexer ===");

    let tokens = selfie::lexer::lex(input).map_err(|e| vec![Error::Lex(e)])?;
    debug!(Lex, "{tokens:#?}\n");

    debug!(Parse, "=== Parser ===");

    let module = selfie::parser::parse_module(tokens, path)
        .map_err(|errs| errs.into_iter().map(Error::Parse).collect::<Vec<_>>())?;

    let mut program = Program {
        modules: vec![module],
    };
    debug!(Parse, "{program:#?}\n");

    debug!(Name, "=== Namer ===\n\n");

    let syms = selfie::namer::name_program(&mut program).map_err(|errs| {
        errs.into_iter()
            .map(|e| Error::Name(Box::new(e)))
            .collect::<Vec<_>>()
    })?;

    debug!(Name, "=== Namer - Symbols ===");
    debug!(Name, "{syms:#?}\n");

    debug!(Name, "=== Namer - Program ===");
    debug!(Name, "{program:#?}\n");

    debug!(CallGraph, "=== Call Graph ===\n\n");

    let cg = program.build_call_graph();

    for fn_decl in program.fns() {
        debug!(
            CallGraph,
            "--- Function - {} ---\n{:#?}\n",
            fn_decl.sym,
            cg.build_fn_info(fn_decl.sym)
        );
    }

    debug!(Type, "=== Typer ===\n\n");

    let ty_ctx = selfie::typer::type_program(&mut program, syms).map_err(|errs| {
        errs.into_iter()
            .map(|e| Error::Type(Box::new(e)))
            .collect::<Vec<_>>()
    })?;

    debug!(Type, "=== Typer - Context ===\n{ty_ctx:#?}\n");
    debug!(Type, "=== Typer - Program ===\n{program:#?}\n");

    Ok(())
}

fn print_errors(path: &Path, input: &str, errors: Vec<Error>) {
    let id = path.display().to_string();

    for e in errors {
        let report = match e {
            Error::Lex(e) => report_lex_error(&e, &id),
            Error::Parse(e) => report_parse_error(&e, &id),
            Error::Name(e) => report_name_error(&e, &id),
            Error::Type(e) => report_typer_error(&e, &id),
        };

        let _ = report.eprint((id.clone(), Source::from(input)));
    }
}
