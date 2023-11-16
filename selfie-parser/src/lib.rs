use std::path::Path;

use chumsky::input::{Input, SpannedInput, Stream};
use chumsky::primitive::just;
use chumsky::span::SimpleSpan;
use chumsky::util::MaybeRef;
use chumsky::{extra, Parser};
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

pub fn parse_module(contents: &str) -> Result<Module, Vec<ParseError>> {
    let tokens = lex(contents).map_err(|e| vec![ParseError::Lex(e)])?;
    let len = tokens.len();

    let tokens = tokens.into_iter().map(|(t, s)| (t, s.into()));
    let input = Stream::from_iter(tokens).spanned((len..len).into());
    let struct_decl = parse_struct().parse(input).into_result();

    Ok(Module {
        decls: vec![Decl::Struct(struct_decl?)],
    })
}

pub fn parse_struct<'a, I>() -> impl Parser<'a, ParserInput<I>, StructDecl, extra::Err<ParseError>>
where
    I: Iterator<Item = Spanned<Token>> + 'a,
{
    just(Token::Equal).map(|_| StructDecl {
        name: Name::new("Hello"),
        fields: vec![],
    })
}

#[cfg(test)]
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
