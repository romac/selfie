use std::ops::Range;

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind};
use selfie_lexer::Token;

use crate::namer::Error as NamerError;
use crate::parser::Error as ParseError;

pub type ReportSpan = (String, Range<usize>);

pub fn parse_error_to_report<'a>(e: &ParseError, id: String) -> Report<'a, ReportSpan> {
    let mut colors = ColorGenerator::new();

    let a = colors.next();
    let b = colors.next();

    match e {
        ParseError::Lex(e) => Report::build(ReportKind::Error, id.clone(), e.span().start)
            .with_code(1)
            .with_message(e.to_string())
            .with_label(
                Label::new((id.clone(), e.span().clone()))
                    .with_message(e.to_string())
                    .with_color(a),
            )
            .finish(),

        ParseError::ExpectedFound {
            span,
            expected,
            found,
        } => {
            let some = expected.iter().filter(|e| e.is_some()).count() > 1;
            let one_of = if some { "one of " } else { "" };

            let found = fmt_found(*found).fg(a);
            let expected = fmt_expected(expected, b).fg(b);

            Report::build(ReportKind::Error, id.clone(), span.start)
                .with_code(2)
                .with_message(format!(
                    "Unexpected token {found}, expected {one_of}{expected}",
                ))
                .with_label(
                    Label::new((id.clone(), span.start..span.end))
                        .with_message(format!("Unexpected token {found}"))
                        .with_color(a),
                )
                .finish()
        }
    }
}

pub fn namer_error_to_report<'a>(e: &NamerError, id: String) -> Report<'a, ReportSpan> {
    let mut colors = ColorGenerator::new();

    let a = colors.next();

    let span = e.span();

    let report = Report::build(ReportKind::Error, id.clone(), span.start)
        .with_code(2)
        .with_message(e.to_string())
        .with_label(
            Label::new((id.clone(), span.start..span.end))
                .with_message(e.to_string())
                .with_color(a),
        );

    let report = if let Some(note) = e.note() {
        report.with_note(note)
    } else {
        report
    };

    report.finish()
}

fn fmt_expected(tokens: &[Option<Token>], color: Color) -> String {
    tokens
        .iter()
        .map(|t| {
            t.map(|t| t.fg(color).to_string())
                .unwrap_or_else(|| "EOF".fg(color).to_string())
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn fmt_found(token: Option<Token>) -> String {
    token
        .map(|t| t.to_string())
        .unwrap_or_else(|| "EOF".to_string())
}
