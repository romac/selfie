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

    #[error("unknown type `{1}`")]
    UnknownType(Span, Sym),

    #[error("duplicate field `{1}`")]
    DuplicateField(Span, Sym),

    #[error("duplicate variant `{1}`")]
    DuplicateVariant(Span, Sym),

    #[error("unexpected argument `{2}`, expected `{3}`")]
    UnexpectedArg(Span, FnSym, Sym, Sym),

    #[error("wrong number of arguments for `{2}`: expected {3}, found {4}")]
    WrongArgCount(Span, FnSym, Sym, usize, usize),

    #[error("extraneous argument label `{2}:` for anonymous parameter `{3}`")]
    ExtraneousArgLabel(Span, FnSym, Sym, Sym),

    #[error("missing argument label `{2}:`")]
    MissingArgLabel(Span, FnSym, Sym),

    #[error("unknown variant `{2}`")]
    UnknownVariant(Span, EnumSym, Sym),

    #[error("missing field `{2}`")]
    MissingField(Span, StructSym, Name),

    #[error("unknown field `{2}`")]
    UnknownField(Span, StructSym, Sym),

    #[error("wrong argument label for `{2}`, expected `{3}`")]
    WrongArgLabel(Span, FnSym, Sym, Sym),
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
            Self::UnknownType(span, _) => *span,
            Self::UnexpectedArg(span, _, _, _) => *span,
            Self::WrongArgCount(span, _, _, _, _) => *span,
            Self::ExtraneousArgLabel(span, _, _, _) => *span,
            Self::MissingArgLabel(span, _, _) => *span,
            Self::DuplicateField(span, _) => *span,
            Self::DuplicateVariant(span, _) => *span,
            Self::UnknownVariant(span, _, _) => *span,
            Self::MissingField(span, _, _) => *span,
            Self::UnknownField(span, _, _) => *span,
            Self::WrongArgLabel(span, _, _, _) => *span,
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
            Self::WrongArgLabel(_, _, arg, alias) => Some(format!(
                "parameter `{arg}` exists but is aliased as `{alias}`"
            )),
            _ => None,
        }
    }

    pub fn help(&self) -> Option<String> {
        match self {
            Self::ExtraneousArgLabel(_, _, arg, _) => {
                Some(format!("consider removing the argument label `{arg}:`"))
            }
            Self::MissingArgLabel(_, _, param) => {
                Some(format!("consider adding an argument label `{param}:`"))
            }
            Self::WrongArgLabel(_, _, arg, expected) => {
                Some(format!("consider changing `{arg}:` to `{expected}:`"))
            }
            _ => None,
        }
    }
}
