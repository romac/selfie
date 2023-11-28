use itertools::Itertools;
use selfie_ast::{Name, Span, Sym};
use thiserror::Error;

use crate::scope::{EnumSym, FnSym, StructSym};

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

    #[error("duplicate field `{1}`")]
    DuplicateField(Span, Sym),

    #[error("duplicate variant `{1}`")]
    DuplicateVariant(Span, Sym),

    #[error("unexpected argument `{2}`, expected `{3}`")]
    UnexpectedArg(Span, FnSym, Sym, Sym),

    #[error("wrong number of arguments for `{2}`: expected {3}, found {4}")]
    WrongArgCount(Span, FnSym, Sym, usize, usize),

    #[error("extraneous argument label `{2}` for anonymous parameter `{3}`")]
    ExtraneousArgLabel(Span, FnSym, Sym, Sym),

    #[error("missing argument label for parameter `{2}`")]
    MissingArgLabel(Span, FnSym, Sym),

    #[error("unknown variant `{2}`")]
    UnknownVariant(Span, EnumSym, Sym),

    #[error("missing field `{2}`")]
    MissingField(Span, StructSym, Name),

    #[error("unknown field `{2}`")]
    UnknownField(Span, StructSym, Sym),
}

fn show_args(fn_sym: &FnSym) -> String {
    fn_sym.params.keys().join(", ")
}

fn show_variants(enum_sym: &EnumSym) -> String {
    enum_sym.variants.keys().join(", ")
}

fn show_fields(struct_sym: &StructSym) -> String {
    struct_sym.fields.keys().join(", ")
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
            Self::UnexpectedArg(span, _, _, _) => *span,
            Self::WrongArgCount(span, _, _, _, _) => *span,
            Self::ExtraneousArgLabel(span, _, _, _) => *span,
            Self::MissingArgLabel(span, _, _) => *span,
            Self::DuplicateField(span, _) => *span,
            Self::DuplicateVariant(span, _) => *span,
            Self::UnknownVariant(span, _, _) => *span,
            Self::MissingField(span, _, _) => *span,
            Self::UnknownField(span, _, _) => *span,
        }
    }

    pub fn note(&self) -> Option<String> {
        match self {
            Self::UnknownField(_, struct_sym, _) => {
                Some(format!("available fields: {}", show_fields(struct_sym)))
            }
            Self::UnknownVariant(_, enum_sym, _) => {
                Some(format!("available variants: {}", show_variants(enum_sym)))
            }
            _ => None,
        }
    }
}
