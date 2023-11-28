use chumsky::error::Rich;
use selfie_lexer::Token;

use crate::Span;

pub type Error = Rich<'static, Token, Span>;
