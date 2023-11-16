use std::path::Path;

use ariadne::{Label, Report, ReportKind, Source};
use selfie_lexer::Token;
use selfie_parser::ParseError;

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

    type Src = (String, selfie::lexer::Span);

    for e in errors {
        let config = ariadne::Config::default();

        match e {
            ParseError::Lex(e) => {
                Report::<Src>::build(ReportKind::Error, id.clone(), e.span().start)
                    .with_config(config)
                    .with_message(e.to_string())
                    .with_label(
                        Label::new((id.clone(), e.span().clone())).with_message(e.to_string()),
                    )
                    .finish()
                    .eprint((id.clone(), Source::from(input)))
                    .unwrap();
            }
            ParseError::ExpectedFound {
                span,
                expected,
                found,
            } => {
                Report::<Src>::build(ReportKind::Error, id.clone(), span.start)
                    .with_config(config)
                    .with_message("Unexpected token")
                    .with_label(
                        Label::new((id.clone(), span.into_range()))
                            .with_message(format!("expected: {}", fmt_expected(&expected))),
                    )
                    .with_label(
                        Label::new((id.clone(), span.into_range()))
                            .with_message(format!("found: {}", fmt_found(found))),
                    )
                    .finish()
                    .eprint((id.clone(), Source::from(input)))
                    .unwrap();
            }
        }
    }
}

fn fmt_expected(tokens: &[Option<Token>]) -> String {
    tokens
        .iter()
        .map(|t| {
            t.map(|t| t.to_string())
                .unwrap_or_else(|| "EOF".to_string())
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn fmt_found(token: Option<Token>) -> String {
    token
        .map(|t| t.to_string())
        .unwrap_or_else(|| "EOF".to_string())
}
