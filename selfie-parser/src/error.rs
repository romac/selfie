use chumsky::util::MaybeRef;
use thiserror::Error;

use selfie_lexer::{Error as LexError, Token};

use crate::{ParserInput, Span};

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("Lexing failed: {0}")]
    Lex(LexError),

    #[error("Expected {expected:?}, found {found:?}")]
    ExpectedFound {
        span: Span,
        expected: Vec<Option<Token>>,
        found: Option<Token>,
    },
}

impl<'a> chumsky::error::Error<'a, ParserInput> for Error {
    fn expected_found<Iter>(expected: Iter, found: Option<MaybeRef<Token>>, span: Span) -> Self
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
