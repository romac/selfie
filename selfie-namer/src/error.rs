use selfie_ast::{Span, Sym};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("duplicate module `{1}`")]
    DuplicateModule(Span, Sym),

    #[error("duplicate param `{1}`")]
    DuplicateParam(Span, Sym),

    #[error("duplicate declaration `{1}`")]
    DuplicateDecl(Span, Sym),

    #[error("unbound variable `{1}`")]
    UnboundVar(Span, Sym),

    #[error("call to unbound function `{1}`")]
    UnboundFn(Span, Sym),

    #[error("unknown type `{0}`")]
    UnknownType(Sym),

    #[error("unexpected argument `{2}`, expected `{1}`")]
    UnexpectedArg(Span, Sym, Sym),

    #[error("wrong number of arguments for `{1}`, expected `{2}`, found `{3}`")]
    WrongArgCount(Span, Sym, usize, usize),

    #[error("extraneous argument label `{1}` for anonymous parameter `{2}`")]
    ExtraneousArgLabel(Span, Sym, Sym),

    #[error("missing argument label for parameter `{1}`")]
    MissingArgLabel(Span, Sym),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::DuplicateModule(span, _) => *span,
            Self::DuplicateParam(span, _) => *span,
            Self::DuplicateDecl(span, _) => *span,
            Self::UnboundVar(span, _) => *span,
            Self::UnboundFn(span, _) => *span,
            Self::UnknownType(_) => todo!(),
            Self::UnexpectedArg(span, _, _) => *span,
            Self::WrongArgCount(span, _, _, _) => *span,
            Self::ExtraneousArgLabel(span, _, _) => *span,
            Self::MissingArgLabel(span, _) => *span,
        }
    }
}
