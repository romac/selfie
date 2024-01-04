use selfie_lexer::Token;

use crate::{Span, Sym};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub span: Span,
    pub sym: Sym,
    pub args: Vec<Token>,
}

impl Attribute {
    pub fn new(span: Span, sym: Sym, args: Vec<Token>) -> Self {
        Self { span, sym, args }
    }
}
