#[derive(Debug)]
pub struct Name(pub(crate) String);

impl Name {
    pub fn is_camel_case(&self) -> bool {
        self.0.chars().next().is_some_and(|c| c.is_uppercase())
    }
}

#[derive(Debug)]
pub enum Type {
    Int64,
    Float64,
    String,
    Bool,
    Unit,
    Named(Name),
    Fn { args: Vec<Type>, ret: Box<Type> },
}

#[derive(Debug)]
pub struct Param {
    pub name: Name,
    pub ty: Type,
    pub anon: bool,
}

#[derive(Debug)]
pub enum Arg {
    Named(NamedArg),
    Anon(Expr),
}

#[derive(Debug)]
pub struct NamedArg {
    pub name: Name,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: Name,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub struct StructDecl {
    pub name: Name,
    pub fields: Vec<Param>,
}

#[derive(Debug)]
pub struct EnumDecl {
    pub name: Name,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub name: Name,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Decl {
    Fn(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
}

#[derive(Debug)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Debug)]
pub enum Op1 {
    Not,
    Neg
}

#[derive(Debug)]
pub struct FnCall {
    pub name: Name,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub struct MethodCall {
    pub receiver: Box<Expr>,
    pub name: Name,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub struct FieldAccess {
    pub receiver: Box<Expr>,
    pub name: Name,
}

#[derive(Debug)]
pub struct Let {
    pub name: Name,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub cnd: Box<Expr>,
    pub thn: Box<Expr>,
    pub els: Box<Expr>,
}

#[derive(Debug)]
pub struct BinOp {
    pub op: Op2,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub op: Op1,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct StructInit {
    pub id: Name,
    pub args: Vec<NamedArg>,
}

#[derive(Debug)]
pub struct EnumInit {
    pub ty: Option<Name>,
    pub variant: Name,
    pub arg: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct Tuple {
    pub items: Vec<Expr>,
}

#[derive(Debug)]
pub enum Literal {
    Int64(i64),
    Float64(f64),
    Bool(bool),
    String(String),
    Unit,
}

#[derive(Debug)]
pub enum Expr {
    Lit(Literal),
    Var(Name),
    FnCall(FnCall),
    MethodCall(MethodCall),
    FieldAccess(FieldAccess),
    Tuple(Tuple),
    Let(Let),
    UnaryOp(UnaryOp),
    BinOp(BinOp),
    If(If),
    StructInit(StructInit),
    EnumInit(EnumInit),
    Return(Box<Expr>),
}
