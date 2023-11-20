use core::fmt;
use std::fmt::Debug;
use std::str::FromStr;

use logos::{Lexer, Logos};
use ordered_float::OrderedFloat;
use thiserror::Error;
use ustr::Ustr;

mod util;
use util::parse_string;

pub use logos::Span;

#[derive(Clone, Debug, Error, PartialEq, Eq, Default)]
pub enum Error {
    #[error("unexpected token: {1}")]
    UnexpectedToken(Span, Ustr),

    #[error("failed to parse int")]
    ParseInt(Span, Ustr),

    #[error("failed to parse float")]
    ParseFloat(Span, Ustr),

    #[error("invalid string literal: {1}")]
    InvalidStringLiteral(Span, Ustr),

    #[default]
    #[error("other")]
    Other,
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::UnexpectedToken(span, _) => span,
            Self::ParseInt(span, _) => span,
            Self::ParseFloat(span, _) => span,
            Self::InvalidStringLiteral(span, _) => span,
            Self::Other => unreachable!(),
        }
    }
}

mod lexers {
    use super::*;

    pub fn auto<T>(lex: &mut Lexer<Token>) -> Option<T>
    where
        T: FromStr,
    {
        lex.slice().parse().ok()
    }

    pub fn string(lex: &mut Lexer<Token>) -> Result<Ustr, Error> {
        parse_string(lex.slice())
            .map(|(_, s)| Ustr::from(&s))
            .map_err(|_| Error::InvalidStringLiteral(lex.span(), Ustr::from(lex.slice())))
    }

    pub fn int(lex: &mut Lexer<Token>) -> Result<i64, Error> {
        lex.slice()
            .parse()
            .map_err(|_| Error::ParseInt(lex.span(), Ustr::from(lex.slice())))
    }

    pub fn float(lex: &mut Lexer<Token>) -> Result<OrderedFloat<f64>, Error> {
        lex.slice()
            .parse()
            .map_err(|_| Error::ParseFloat(lex.span(), Ustr::from(lex.slice())))
    }

    // pub fn comment(lex: &mut Lexer<Token>) -> Option<Ustr> {
    //     Some(Ustr::from(lex.slice().trim_start_matches(['/', ' '])))
    // }

    pub fn intern(lex: &mut Lexer<Token>) -> Option<Ustr> {
        Some(Ustr::from(lex.slice()))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Logos)]
#[logos(skip r"[ \t\n\f]+", error = Error)]
pub enum Token {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", lexers::intern)]
    Identifier(Ustr),

    #[regex("-?[0-9]+", lexers::int)]
    Int64(i64),

    #[regex("(-?NaN)|(-?[0-9]+\\.[0-9]+)", lexers::float)]
    Float64(OrderedFloat<f64>),

    #[regex("true|false", lexers::auto)]
    Bool(bool),

    #[regex("\"([^\"\\]|\\[.])*\"", lexers::string)]
    String(Ustr),

    #[regex("//[^\n]*", logos::skip)]
    Comment,

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

    #[token(";")]
    Semi,

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

    #[token("%")]
    Percent,

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
    OrOr,

    #[token("&&")]
    AndAnd,

    #[token("|")]
    Or,

    #[token("&")]
    And,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;

        match self {
            Identifier(s) => write!(f, "{s}"),
            Int64(i) => write!(f, "{i}"),
            Float64(i) => write!(f, "{i}"),
            Bool(b) => write!(f, "{b}"),
            String(s) => write!(f, "\"{s}\""),
            Comment => Ok(()),
            Fn => write!(f, "fn"),
            Struct => write!(f, "struct"),
            Enum => write!(f, "enum"),
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            ParenOpen => write!(f, "("),
            ParenClose => write!(f, ")"),
            BraceOpen => write!(f, "{{"),
            BraceClose => write!(f, "}}"),
            Colon => write!(f, ":"),
            Semi => write!(f, ";"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Arrow => write!(f, "->"),
            Under => write!(f, "_"),
            Slash => write!(f, "/"),
            Star => write!(f, "*"),
            Dash => write!(f, "-"),
            Plus => write!(f, "+"),
            Percent => write!(f, "%"),
            Bang => write!(f, "!"),
            BangEqual => write!(f, "!="),
            Equal => write!(f, "="),
            EqualEqual => write!(f, "=="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            OrOr => write!(f, "||"),
            AndAnd => write!(f, "&&"),
            Or => write!(f, "|"),
            And => write!(f, "&"),
        }
    }
}

pub fn lex(input: &str) -> Result<Vec<(Token, Span)>, Error> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        match token {
            Ok(token) => tokens.push((token, lexer.span())),

            Err(Error::Other) => {
                return Err(Error::UnexpectedToken(
                    lexer.span(),
                    Ustr::from(lexer.slice()),
                ));
            }

            Err(err) => return Err(err),
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    fn toks(input: &str) -> Result<Vec<Token>, Error> {
        Ok(lex(input)?.into_iter().map(|(t, _)| t).collect::<Vec<_>>())
    }

    fn identifier(s: &str) -> Token {
        Token::Identifier(Ustr::from(s))
    }

    fn string(s: &str) -> Token {
        Token::String(Ustr::from(s))
    }

    #[test]
    fn lex_invalid() {
        let tokens = toks("hello_world 42 \"foo").unwrap_err();
        assert_eq!(tokens, Error::UnexpectedToken(15..19, Ustr::from("\"foo"),));

        let tokens = toks("trabad%toto");
        assert_eq!(tokens, Err(Error::UnexpectedToken(6..7, Ustr::from("%"))));

        let input = "\"foo\\u{}bar\"";
        let tokens = toks(input).unwrap_err();
        assert_eq!(
            tokens,
            Error::InvalidStringLiteral(0..12, Ustr::from(input))
        );

        let input = "999999999912491943249329493294932492342347235476236426462348324673264823";
        let tokens = toks(input).unwrap_err();
        assert_eq!(tokens, Error::ParseInt(0..72, Ustr::from(input)));
    }

    #[test]
    fn lex_multi() {
        let tokens = toks("42 hello_world -123").unwrap();
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
        let tokens = toks("42").unwrap();
        assert_eq!(tokens, vec![Token::Int64(42)]);
    }

    #[test]
    fn lex_string_escaped() {
        let tokens = toks("\"foo\\nbar\"").unwrap();
        assert_eq!(tokens, vec![string("foo\nbar")]);

        let tokens = toks("\"foo\\\\bar\"").unwrap();
        assert_eq!(tokens, vec![string("foo\\bar")]);

        let tokens = toks("\"foo\\u{7FFF}bar\"").unwrap();
        assert_eq!(tokens, vec![string("foo翿bar")]);

        let tokens = toks("\"foo翿bar\"").unwrap();
        assert_eq!(tokens, vec![string("foo翿bar")]);

        let tokens = toks("\"foo\\u{1F60D}bar\"").unwrap();
        assert_eq!(tokens, vec![string("foo😍bar")]);

        let tokens = toks("\"foo😍bar\"").unwrap();
        assert_eq!(tokens, vec![string("foo😍bar")]);

        let tokens = toks("\"foo\\    bar\"").unwrap();
        assert_eq!(tokens, vec![string("foobar")]);
    }

    #[test]
    fn lex_float64() {
        let tokens = toks("42.69").unwrap();
        assert_eq!(tokens, vec![Token::Float64(42.69f64.into())]);

        let tokens = toks("-42.69").unwrap();
        assert_eq!(tokens, vec![Token::Float64((-42.69f64).into())]);
    }

    #[test]
    fn lex_identifier() {
        let tokens = toks("hello_world").unwrap();
        assert_eq!(tokens, vec![identifier("hello_world")]);

        let tokens = toks("_helloWorld").unwrap();
        assert_eq!(tokens, vec![identifier("_helloWorld")]);
    }

    #[test]
    fn lex_program() {
        use Token::*;

        let tokens = toks(
            r#"
            fn add(_ x: Int64, y: Int64) -> Int64 {
                let x = 42
                let y = -69
                let z = "Hello, World!"
                true
            }
        "#,
        )
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
                string("Hello, World!"),
                Bool(true),
                BraceClose
            ]
        );
    }
}
