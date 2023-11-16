use std::fmt::Debug;
use std::str::FromStr;

use logos::{Lexer, Logos, Span};

use ordered_float::OrderedFloat;
use thiserror::Error;

mod util;
use util::parse_string;

fn auto<T>(lex: &mut Lexer<Token>) -> Option<T>
where
    T: FromStr,
{
    lex.slice().parse().ok()
}

fn lex_string(lex: &mut Lexer<Token>) -> Option<String> {
    parse_string(lex.slice()).ok().map(|(_, s)| s)
}

#[derive(Clone, Debug, PartialEq, Eq, Logos)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", auto)]
    Identifier(String),

    #[regex("-?[0-9]+", auto)]
    Int64(i64),

    #[regex("-?[0-9]+\\.[0-9]+", auto)]
    Float64(OrderedFloat<f64>),

    #[regex("true|false", auto)]
    Bool(bool),

    #[regex("\"([^\"\\]|\\[.])*\"", lex_string)]
    String(String),

    #[token("fn")]
    Fn,

    #[token("struct")]
    Struct,

    #[token("enum")]
    Enum,

    #[token("let")]
    Let,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("->")]
    Arrow,

    #[token("_")]
    Under,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    #[token("-")]
    Dash,

    #[token("+")]
    Plus,

    #[token("!")]
    Bang,

    #[token("!=")]
    BangEqual,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEqual,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEqual,

    #[token("||")]
    Or,

    #[token("&&")]
    And,
}

#[derive(Debug, Error)]
pub enum LexError<'input> {
    #[error("unexpected token: {1}")]
    UnexpectedToken(Span, &'input str),
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        if let Ok(token) = token {
            tokens.push(token);
        } else {
            return Err(LexError::UnexpectedToken(lexer.span(), lexer.slice()));
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    fn identifier(s: &str) -> Token {
        Token::Identifier(s.to_string())
    }

    #[test]
    fn lex_multi() {
        let tokens = lex("42 hello_world -123").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Int64(42),
                identifier("hello_world"),
                Token::Int64(-123)
            ]
        );
    }

    #[test]
    fn lex_int64() {
        let tokens = lex("42").unwrap();
        assert_eq!(tokens, vec![Token::Int64(42)]);
    }

    #[test]
    fn lex_string_escaped() {
        let tokens = lex("\"foo\\nbar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foo\nbar".to_string())]);

        let tokens = lex("\"foo\\\\bar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foo\\bar".to_string())]);

        let tokens = lex("\"foo\\u{7FFF}bar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foo翿bar".to_string())]);

        let tokens = lex("\"foo翿bar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foo翿bar".to_string())]);

        let tokens = lex("\"foo\\u{1F60D}bar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foo😍bar".to_string())]);

        let tokens = lex("\"foo😍bar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foo😍bar".to_string())]);

        let tokens = lex("\"foo\\    bar\"").unwrap();
        assert_eq!(tokens, vec![Token::String("foobar".to_string())]);
    }

    #[test]
    fn lex_float64() {
        let tokens = lex("42.69").unwrap();
        assert_eq!(tokens, vec![Token::Float64(42.69f64.into())]);

        let tokens = lex("-42.69").unwrap();
        assert_eq!(tokens, vec![Token::Float64((-42.69f64).into())]);
    }

    #[test]
    fn lex_identifier() {
        let tokens = lex("hello_world").unwrap();
        assert_eq!(tokens, vec![identifier("hello_world")]);

        let tokens = lex("_helloWorld").unwrap();
        assert_eq!(tokens, vec![identifier("_helloWorld")]);
    }

    #[test]
    fn lex_program() {
        use Token::*;

        let tokens = lex(r#"
            fn add(_ x: Int64, y: Int64) -> Int64 {
                let x = 42
                let y = -69
                let z = "Hello, World!"
                true
            }
        "#)
        .unwrap();

        assert_eq!(
            tokens,
            vec![
                Fn,
                identifier("add"),
                ParenOpen,
                Under,
                identifier("x"),
                Colon,
                identifier("Int64"),
                Comma,
                identifier("y"),
                Colon,
                identifier("Int64"),
                ParenClose,
                Arrow,
                identifier("Int64"),
                BraceOpen,
                Let,
                identifier("x"),
                Equal,
                Int64(42),
                Let,
                identifier("y"),
                Equal,
                Int64(-69),
                Let,
                identifier("z"),
                Equal,
                String("Hello, World!".to_string()),
                Bool(true),
                BraceClose
            ]
        );
    }
}
