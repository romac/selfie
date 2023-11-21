use crate::{Expr, Name, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub name: Name,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Param {
    pub name: Name,
    pub ty: Type,
    pub kind: ParamKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParamKind {
    Anon,
    Normal,
    Alias(Name),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDecl {
    pub name: Name,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumDecl {
    pub name: Name,
    pub variants: Vec<Variant>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variant {
    pub name: Name,
    pub ty: Option<Type>,
}
