use crate::{impl_decl, impl_span, impl_sym, Attribute, Expr, Span, Sym, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Impl(ImplDecl),
}

impl Decl {
    pub fn sym(&self) -> Sym {
        match self {
            Self::Fn(fn_decl) => fn_decl.sym(),
            Self::Struct(struct_decl) => struct_decl.sym(),
            Self::Enum(enum_decl) => enum_decl.sym(),
            Self::Impl(impl_decl) => impl_decl.sym(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Fn(fn_decl) => fn_decl.span(),
            Self::Struct(struct_decl) => struct_decl.span(),
            Self::Enum(enum_decl) => enum_decl.span(),
            Self::Impl(impl_decl) => impl_decl.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub span: Span,
    pub sym: Sym,
    pub attrs: Vec<Attribute>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Expr,
}

impl_decl!(FnDecl);

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

impl ParamKind {
    #[must_use]
    pub fn is_anon(&self) -> bool {
        matches!(self, Self::Anon)
    }

    #[must_use]
    pub fn is_normal(&self) -> bool {
        matches!(self, Self::Normal)
    }

    #[must_use]
    pub fn is_alias(&self) -> bool {
        matches!(self, Self::Alias(..))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDecl {
    pub span: Span,
    pub sym: Sym,
    pub attrs: Vec<Attribute>,
    pub fields: Vec<Field>,
}

impl_decl!(StructDecl);

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
    pub attrs: Vec<Attribute>,
    pub variants: Vec<Variant>,
}

impl_decl!(EnumDecl);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variant {
    pub span: Span,
    pub sym: Sym,
    pub ty: Option<Type>,
}

impl_sym!(Variant);
impl_span!(Variant);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplDecl {
    pub span: Span,
    pub sym: Sym,
    pub methods: Vec<FnDecl>,
}

impl_span!(ImplDecl);
impl_sym!(ImplDecl);
