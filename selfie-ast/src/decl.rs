use crate::{impl_span, impl_sym, Expr, Span, Sym, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
}

impl Decl {
    pub fn sym(&self) -> Sym {
        match self {
            Self::Fn(fn_decl) => fn_decl.sym(),
            Self::Struct(struct_decl) => struct_decl.sym(),
            Self::Enum(enum_decl) => enum_decl.sym(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Fn(fn_decl) => fn_decl.span(),
            Self::Struct(struct_decl) => struct_decl.span(),
            Self::Enum(enum_decl) => enum_decl.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub span: Span,
    pub sym: Sym,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Expr,
}

impl_sym!(FnDecl);
impl_span!(FnDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Param {
    pub span: Span,
    pub sym: Sym,
    pub ty: Type,
    pub kind: ParamKind,
}

impl_sym!(Param);
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

impl_sym!(StructDecl);
impl_span!(StructDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Field {
    pub span: Span,
    pub sym: Sym,
    pub ty: Type,
}

impl_sym!(Field);
impl_span!(Field);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumDecl {
    pub span: Span,
    pub sym: Sym,
    pub variants: Vec<Variant>,
}

impl_sym!(EnumDecl);
impl_span!(EnumDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variant {
    pub span: Span,
    pub sym: Sym,
    pub ty: Option<Type>,
}

impl_sym!(Variant);
impl_span!(Variant);
