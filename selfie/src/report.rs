use std::ops::Range;

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind};
use chumsky::error::RichPattern;
use selfie_lexer::Token;

use crate::lexer::Error as LexError;
use crate::namer::Error as NamerError;
use crate::parser::Error as ParseError;

pub type ReportSpan = (String, Range<usize>);

pub fn lex_error_to_report<'a>(e: &LexError, id: &str) -> Report<'a, ReportSpan> {
    let mut colors = ColorGenerator::new();

    let a = colors.next();
    let span = e.span();

    Report::build(ReportKind::Error, id.to_string(), span.start)
        .with_code(1)
        .with_message(e.to_string())
        .with_label(
            Label::new((id.to_string(), span.start..span.end))
                .with_message(e.to_string())
                .with_color(a),
        )
        .finish()
}

pub fn parse_error_to_report<'a>(e: &ParseError, id: &str) -> Report<'a, ReportSpan> {
    let mut colors = ColorGenerator::new();
    let fg1 = colors.next();
    let fg2 = colors.next();
    let fg3 = colors.next();

    let span = e.span();
    let expected = e.expected();
    let found = e.found();

    let one_of = if expected.len() > 1 { "one of " } else { "" };

    let found = fmt_found(found).fg(fg1);
    let expected = fmt_expected(expected, fg2).fg(fg2);

    let mut report = Report::build(ReportKind::Error, id.to_string(), span.start)
        .with_code(2)
        .with_message(format!(
            "Unexpected token {found}, expected {one_of}{expected}",
        ))
        .with_label(
            Label::new((id.to_string(), span.start..span.end))
                .with_message(format!("Unexpected token {found}"))
                .with_color(fg1),
        );

    for (label, span) in e.contexts() {
        report.add_label(
            Label::new((id.to_string(), span.start..span.end))
                .with_message(label)
                .with_color(fg3),
        );
    }

    report.finish()
}

pub fn namer_error_to_report<'a>(e: &NamerError, id: &str) -> Report<'a, ReportSpan> {
    let mut colors = ColorGenerator::new();
    let fg = colors.next();

    let span = e.span();

    let mut report = Report::build(ReportKind::Error, id.to_string(), span.start)
        .with_code(2)
        .with_message(e.to_string())
        .with_label(
            Label::new((id.to_string(), span.start..span.end))
                .with_message(e.to_string())
                .with_color(fg),
        );

    if let Some(note) = e.note() {
        report.set_note(note);
    }

    if let Some(help) = e.help() {
        report.set_help(help);
    }

    report.finish()
}

fn fmt_expected<'a>(
    tokens: impl ExactSizeIterator<Item = &'a RichPattern<'a, Token>>,
    color: Color,
) -> String {
    tokens
        .map(|t| match t {
            RichPattern::Token(t) => t.fg(color).to_string(),
            RichPattern::Label(l) => l.fg(color).to_string(),
            RichPattern::EndOfInput => "end of input".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn fmt_found(token: Option<&Token>) -> String {
    token
        .map(|t| t.to_string())
        .unwrap_or_else(|| "end of input".to_string())
}
