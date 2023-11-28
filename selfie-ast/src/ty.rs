use crate::{Span, Sym};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int64,
    Float64,
    String,
    Bool,
    Unit,
    Tuple(Vec<Type>),
    Named(Span, Sym),
    Fn { args: Vec<Type>, ret: Box<Type> },
}
