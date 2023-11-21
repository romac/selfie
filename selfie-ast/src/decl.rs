use crate::{impl_span, Expr, Span, Sym, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub span: Span,
    pub sym: Sym,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Expr,
}

impl_span!(FnDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Param {
    pub span: Span,
    pub sym: Sym,
    pub ty: Type,
    pub kind: ParamKind,
}

impl_span!(Param);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParamKind {
    Anon,
    Normal,
    Alias(Sym),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDecl {
    pub span: Span,
    pub sym: Sym,
    pub fields: Vec<Field>,
}

impl_span!(StructDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Field {
    pub span: Span,
    pub sym: Sym,
    pub ty: Type,
}

impl_span!(Field);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumDecl {
    pub span: Span,
    pub sym: Sym,
    pub variants: Vec<Variant>,
}

impl_span!(EnumDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variant {
    pub span: Span,
    pub sym: Sym,
    pub ty: Option<Type>,
}

impl_span!(Variant);
