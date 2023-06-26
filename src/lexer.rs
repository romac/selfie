#![allow(dead_code)]

use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1, multispace0},
    combinator::{map, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, pair, tuple},
    IResult,
};

mod string;
use string::parse_string;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Int64(i64),
    Float64(f64),
    Bool(bool),
    String(String),
    Fn,
    Struct,
    Enum,
    Let,
    If,
    Else,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Colon,
    Comma,
    Equals,
    Arrow,
    Under,
}

#[derive(Debug)]
pub enum LexError<'input> {
    Nom(nom::error::Error<&'input str>),
}

impl<'input> fmt::Display for LexError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::Nom(e) => write!(f, "{e}"),
        }
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let (remaining, tokens) = many1(lex_token)(input).unwrap(); //.finish().map_err(LexError::Nom)?;
    assert_eq!(remaining, "");
    Ok(tokens)
}

fn lex_identifier(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(alpha1, many0(alt((alpha1, digit1, tag("_")))))),
        |s: &str| Token::Identifier(s.to_string()),
    )(input)
}

fn lex_float64(input: &str) -> IResult<&str, Token> {
    map(recognize(tuple((digit1, tag("."), digit1))), |s: &str| {
        Token::Float64(s.parse().unwrap())
    })(input)
}

fn lex_int64(input: &str) -> IResult<&str, Token> {
    map(digit1, |s: &str| Token::Int64(s.parse().unwrap()))(input)
}

fn lex_string(input: &str) -> IResult<&str, Token> {
    map(parse_string, Token::String)(input)
}

fn lex_bool(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Bool(true), tag("true")),
        value(Token::Bool(false), tag("false")),
    ))(input)
}

fn lex_fn(input: &str) -> IResult<&str, Token> {
    value(Token::Fn, tag("fn"))(input)
}

fn lex_struct(input: &str) -> IResult<&str, Token> {
    value(Token::Struct, tag("struct"))(input)
}

fn lex_enum(input: &str) -> IResult<&str, Token> {
    value(Token::Enum, tag("enum"))(input)
}

fn lex_let(input: &str) -> IResult<&str, Token> {
    value(Token::Let, tag("let"))(input)
}

fn lex_if(input: &str) -> IResult<&str, Token> {
    value(Token::If, tag("if"))(input)
}

fn lex_else(input: &str) -> IResult<&str, Token> {
    value(Token::Else, tag("else"))(input)
}

fn lex_open_paren(input: &str) -> IResult<&str, Token> {
    value(Token::ParenOpen, tag("("))(input)
}

fn lex_close_paren(input: &str) -> IResult<&str, Token> {
    value(Token::ParenClose, tag(")"))(input)
}

fn lex_open_brace(input: &str) -> IResult<&str, Token> {
    value(Token::BraceOpen, tag("{"))(input)
}

fn lex_close_brace(input: &str) -> IResult<&str, Token> {
    value(Token::BraceClose, tag("}"))(input)
}

fn lex_colon(input: &str) -> IResult<&str, Token> {
    value(Token::Colon, tag(":"))(input)
}

fn lex_comma(input: &str) -> IResult<&str, Token> {
    value(Token::Comma, tag(","))(input)
}

fn lex_equals(input: &str) -> IResult<&str, Token> {
    value(Token::Equals, tag("="))(input)
}

fn lex_arrow(input: &str) -> IResult<&str, Token> {
    value(Token::Arrow, tag("->"))(input)
}

fn lex_under(input: &str) -> IResult<&str, Token> {
    value(Token::Under, tag("_"))(input)
}

fn lex_token(input: &str) -> IResult<&str, Token> {
    delimited(
        multispace0,
        alt((
            lex_fn,
            lex_struct,
            lex_enum,
            lex_let,
            lex_if,
            lex_else,
            lex_open_paren,
            lex_close_paren,
            lex_open_brace,
            lex_close_brace,
            lex_colon,
            lex_comma,
            lex_equals,
            lex_under,
            lex_arrow,
            lex_bool,
            lex_identifier,
            lex_float64,
            lex_int64,
            // lex_string,
        )),
        multispace0,
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn identifier(s: &str) -> Token {
        Token::Identifier(s.to_string())
    }

    #[test]
    fn lex_multi() {
        let tokens = lex("42 hello_world 123.456").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Int64(42),
                identifier("hello_world"),
                Token::Float64(123.456)
            ]
        );
    }

    #[test]
    fn lex_int64() {
        let tokens = lex("42").unwrap();
        assert_eq!(tokens, vec![Token::Int64(42)]);
    }

    #[test]
    fn lex_float64() {
        let tokens = lex("42.69").unwrap();
        assert_eq!(tokens, vec![Token::Float64(42.69)]);
    }

    #[test]
    fn lex_identifier() {
        let tokens = lex("hello_world").unwrap();
        assert_eq!(tokens, vec![identifier("hello_world")]);
    }

    #[test]
    fn lex_string() {
        let tokens = vec![super::lex_string(r#""Hello World""#).unwrap().1];
        assert_eq!(tokens, vec![Token::String(String::from("Hello World"))]);
    }

    #[test]
    fn lex_program() {
        use Token::*;

        let tokens = lex(r#"
            fn add(_ x: Int64, y: Int64) -> Int64 {
                let x = 42
                let y = 69.123
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
                Equals,
                Int64(42),
                Let,
                identifier("y"),
                Equals,
                Float64(69.123),
                // Let,
                // identifier("z"),
                // Equals,
                // String("Hello World".to_string()),
                Bool(true),
                BraceClose
            ]
        );
    }
}
