use core::fmt;
use std::hash::Hasher;

use indexmap::IndexMap;

use selfie_ast::symbols::Symbols;
use selfie_ast::{EnumDecl, Expr, FnDecl, StructDecl, Sym};

use crate::ty::{EnumType, FnType, StructType};
use crate::Type;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExprId(u64);

impl ExprId {
    pub fn new(expr: &Expr) -> Self {
        use std::hash::Hash;

        let mut hasher = std::hash::DefaultHasher::new();
        expr.hash(&mut hasher);
        let hash = hasher.finish();
        Self(hash)
    }
}

pub struct TyCtx {
    _syms: Symbols,
    vars: IndexMap<Sym, Type>,
    fns: IndexMap<Sym, FnType>,
    structs: IndexMap<Sym, StructType>,
    enums: IndexMap<Sym, EnumType>,
    exprs: IndexMap<ExprId, Type>,
}

impl TyCtx {
    pub fn new(_syms: Symbols) -> Self {
        Self {
            _syms,
            vars: IndexMap::new(),
            fns: IndexMap::new(),
            structs: IndexMap::new(),
            enums: IndexMap::new(),
            exprs: IndexMap::new(),
        }
    }

    pub fn add_var(&mut self, sym: Sym, ty: Type) {
        self.vars.insert(sym, ty);
    }

    pub fn get_var(&self, sym: Sym) -> Option<&Type> {
        self.vars.get(&sym)
    }

    pub fn add_fn(&mut self, sym: Sym, ty: FnType) {
        self.fns.insert(sym, ty);
    }

    pub fn get_fn(&self, sym: Sym) -> Option<&FnType> {
        self.fns.get(&sym)
    }

    pub fn add_expr(&mut self, expr: &Expr, ty: Type) {
        let id = ExprId::new(expr);
        self.exprs.insert(id, ty);
    }

    pub fn get_expr(&self, expr: &Expr) -> Option<&Type> {
        let id = ExprId::new(expr);
        self.exprs.get(&id)
    }

    pub fn add_struct(&mut self, sym: Sym, ty: StructType) {
        self.structs.insert(sym, ty);
    }

    pub fn get_struct(&self, sym: Sym) -> Option<&StructType> {
        self.structs.get(&sym)
    }

    pub fn add_enum(&mut self, sym: Sym, ty: EnumType) {
        self.enums.insert(sym, ty);
    }

    pub fn get_enum(&self, sym: Sym) -> Option<&EnumType> {
        self.enums.get(&sym)
    }

    pub fn add_fn_decl(&mut self, sym: Sym, fn_decl: &FnDecl) {
        let fn_ty = FnType {
            args: fn_decl
                .params
                .iter()
                .map(|param| Type::from(&param.ty))
                .collect(),

            ret: Box::new(Type::from(&fn_decl.return_type)),
        };

        self.add_fn(sym, fn_ty);
    }

    pub fn add_struct_decl(&mut self, struct_decl: &StructDecl) {
        let struct_ty = StructType {
            fields: struct_decl
                .fields
                .iter()
                .map(|field| (field.sym, Type::from(&field.ty)))
                .collect(),
        };

        self.add_struct(struct_decl.sym, struct_ty);
    }

    pub fn add_enum_decl(&mut self, enum_decl: &EnumDecl) {
        let enum_ty = EnumType {
            variants: enum_decl
                .variants
                .iter()
                .map(|variant| (variant.sym, variant.ty.as_ref().map(Type::from)))
                .collect(),
        };

        self.add_enum(enum_decl.sym, enum_ty);
    }
}

impl fmt::Debug for TyCtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let alt = f.alternate();
        let mut d = f.debug_struct("TyCtx");
        d.field("vars", &self.vars);
        d.field("fns", &self.fns);
        d.field("structs", &self.structs);
        d.field("enums", &self.enums);
        // if alt {
        //     d.field("exprs", &self.exprs);
        // }
        d.finish()
    }
}
