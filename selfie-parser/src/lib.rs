use std::path::Path;

use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::primitive::{end, just};
use chumsky::span::SimpleSpan;
use chumsky::util::MaybeRef;
use chumsky::{extra, select, IterParser, Parser};
use thiserror::Error;

use selfie_ast::*;
use selfie_lexer::{lex, LexError, Token};

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Lexing failed: {0}")]
    Lex(LexError),

    #[error("Expected {expected:?}, found {found:?}")]
    ExpectedFound {
        span: Span,
        expected: Vec<Option<Token>>,
        found: Option<Token>,
    },
}

impl<'a, I> chumsky::error::Error<'a, ParserInput<I>> for ParseError
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    fn expected_found<Iter>(expected: Iter, found: Option<MaybeRef<'a, Token>>, span: Span) -> Self
    where
        Iter: IntoIterator<Item = Option<MaybeRef<'a, Token>>>,
    {
        Self::ExpectedFound {
            span,
            expected: expected
                .into_iter()
                .map(|e| e.as_deref().copied())
                .collect(),
            found: found.as_deref().copied(),
        }
    }

    fn merge(mut self, mut other: Self) -> Self {
        if let (
            Self::ExpectedFound { expected, .. },
            Self::ExpectedFound {
                expected: expected_other,
                ..
            },
        ) = (&mut self, &mut other)
        {
            expected.append(expected_other);
        }
        self
    }
}

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type ParserInput<I> = SpannedInput<Token, Span, Stream<I>>;

pub fn parse_file(path: impl AsRef<Path>) -> Result<Module, Vec<ParseError>> {
    let contents = std::fs::read_to_string(path).unwrap();
    parse_module(&contents)
}

fn text_to_input(
    text: &str,
) -> Result<ParserInput<impl Iterator<Item = Spanned<Token>>>, LexError> {
    let tokens = lex(text)?;
    let len = tokens.len();
    let tokens = tokens.into_iter().map(|(t, s)| (t, s.into()));
    Ok(Stream::from_iter(tokens).spanned((len..len).into()))
}

pub fn parse_module(contents: &str) -> Result<Module, Vec<ParseError>> {
    let input = text_to_input(contents).map_err(|e| vec![ParseError::Lex(e)])?;

    let decls = parse_decl()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .parse(input)
        .into_result()?;

    Ok(Module { decls })
}

pub fn parse_identifier<'a, I>() -> impl Parser<'a, ParserInput<I>, Name, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    select!(Token::Identifier(id) => Name::interned(id))
}

pub fn parse_type<'a, I>() -> impl Parser<'a, ParserInput<I>, Type, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    fn to_type(id: Ustr) -> Type {
        match id.as_ref() {
            "String" => Type::String,
            "Bool" => Type::Bool,
            "Unit" => Type::Unit,
            "Int64" => Type::Int64,
            "Float64" => Type::Float64,
            _ => Type::Named(Name::interned(id)),
        }
    }

    select!(Token::Identifier(id) => to_type(id))
}

pub fn parse_decl<'a, I>() -> impl Parser<'a, ParserInput<I>, Decl, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    parse_struct()
        .map(Decl::Struct)
        .or(parse_enum().map(Decl::Enum))
}

pub fn parse_struct<'a, I>() -> impl Parser<'a, ParserInput<I>, StructDecl, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    let fields = parse_field()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Struct)
        .ignore_then(parse_identifier())
        .then(braces(fields))
        .map(|(name, fields)| StructDecl { name, fields })
}

pub fn parse_field<'a, I>() -> impl Parser<'a, ParserInput<I>, Param, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    parse_identifier()
        .then_ignore(just(Token::Colon))
        .then(parse_type())
        .map(|(name, ty)| Param {
            name,
            ty,
            anon: false,
        })
}

pub fn parse_enum<'a, I>() -> impl Parser<'a, ParserInput<I>, EnumDecl, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    let variants = parse_variant()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    just(Token::Enum)
        .ignore_then(parse_identifier())
        .then(braces(variants))
        .map(|(name, variants)| EnumDecl { name, variants })
}

pub fn parse_variant<'a, I>() -> impl Parser<'a, ParserInput<I>, Variant, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    just(Token::Dot)
        .ignore_then(parse_identifier())
        .then(parens(parse_type()))
        .map(|(name, ty)| Variant { name, ty })
}

fn braces<'a, A, I>(
    p: impl Parser<'a, ParserInput<I>, A, extra::Err<ParseError>>,
) -> impl Parser<'a, ParserInput<I>, A, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    p.delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
}

fn parens<'a, A, I>(
    p: impl Parser<'a, ParserInput<I>, A, extra::Err<ParseError>>,
) -> impl Parser<'a, ParserInput<I>, A, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    p.delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
}

#[cfg(test)]
#[allow(unused_variables)]
mod tests {
    use super::*;

    #[test]
    fn parse_examples() {
        let examples = std::fs::read_dir("../examples").unwrap();
        for example in examples {
            let example = example.unwrap();
            let path = example.path();
            if path.extension().unwrap() == "self" {
                println!("Parsing {:?}", path);

                match parse_file(path) {
                    Ok(module) => println!("{module:#?}"),
                    Err(es) => {
                        for e in es {
                            eprintln!("{e}");
                        }
                        panic!("one or more error occured");
                    }
                }
            }
        }
    }

    #[test]
    fn parse_braces() {
        let input = text_to_input(r#" { hello_world }"#).unwrap();
        let expr = braces(parse_identifier()).parse(input).unwrap();
    }

    #[test]
    fn parse_sep_by() {
        let input = text_to_input(r#"foo, bar"#).unwrap();
        let expr = parse_identifier()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .parse(input)
            .unwrap();

        let input = text_to_input(r#"foo, bar,"#).unwrap();
        let expr = parse_identifier()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .parse(input)
            .unwrap();

        let input = text_to_input(r#" { foo, bar, }"#).unwrap();
        let expr = braces(
            parse_identifier()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .parse(input)
        .unwrap();

        let input = text_to_input(r#" { foo: Int64, bar: String, }"#).unwrap();
        let expr = braces(
            parse_field()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .parse(input)
        .unwrap();
    }

    // #[test]
    // fn parse_arith() {
    //     let mut tokens = lex(r#" x + 2 * y - 3 / 4 + z"#)
    //         .unwrap()
    //         .into_iter()
    //         .peekable();
    //     let expr = parse_expr(&mut tokens).unwrap();
    //     dbg!(&tokens);
    //     println!("{:#?}", expr);
    // }
}
