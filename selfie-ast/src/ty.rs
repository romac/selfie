use crate::{Span, Sym};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int64(Span),
    Float64(Span),
    String(Span),
    Bool(Span),
    Unit(Span),
    Tuple(Span, Vec<Type>),
    Named(Span, Sym),
    Fn {
        span: Span,
        args: Vec<Type>,
        ret: Box<Type>,
    },
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Int64(span) => *span,
            Type::Float64(span) => *span,
            Type::String(span) => *span,
            Type::Bool(span) => *span,
            Type::Unit(span) => *span,
            Type::Tuple(span, _) => *span,
            Type::Named(span, _) => *span,
            Type::Fn { span, .. } => *span,
        }
    }
}
