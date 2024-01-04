use indexmap::IndexMap;

use crate::{Name, ParamKind, Span, Sym, Type};

#[derive(Default, Debug)]
pub struct Symbols {
    pub vars: IndexMap<Name, Sym>,
    pub fns: IndexMap<Name, FnSym>,
    pub structs: IndexMap<Name, StructSym>,
    pub enums: IndexMap<Name, EnumSym>,
    pub cur_impl: Option<Sym>,
}

impl Symbols {
    pub fn add_var(&mut self, sym: Sym) {
        self.vars.insert(sym.name, sym);
    }

    pub fn get_var(&self, name: &Name) -> Option<&Sym> {
        self.vars.get(name)
    }

    pub fn get_var_mut(&mut self, name: &Name) -> Option<&mut Sym> {
        self.vars.get_mut(name)
    }

    pub fn add_fn(&mut self, fn_sym: FnSym) {
        self.fns.insert(fn_sym.sym.name, fn_sym);
    }

    pub fn get_fn(&self, name: &Name) -> Option<&FnSym> {
        self.fns.get(name)
    }

    pub fn get_fn_mut(&mut self, name: &Name) -> Option<&mut FnSym> {
        self.fns.get_mut(name)
    }

    pub fn add_struct(&mut self, struct_sym: StructSym) {
        self.structs.insert(struct_sym.sym.name, struct_sym);
    }

    pub fn get_struct(&self, name: &Name) -> Option<&StructSym> {
        self.structs.get(name)
    }

    pub fn get_struct_mut(&mut self, name: &Name) -> Option<&mut StructSym> {
        self.structs.get_mut(name)
    }

    pub fn add_enum(&mut self, enum_sym: EnumSym) {
        self.enums.insert(enum_sym.sym.name, enum_sym);
    }

    pub fn get_enum(&self, name: &Name) -> Option<&EnumSym> {
        self.enums.get(name)
    }

    pub fn get_enum_mut(&mut self, name: &Name) -> Option<&mut EnumSym> {
        self.enums.get_mut(name)
    }
}

#[derive(Clone, Debug)]
pub struct FnSym {
    pub sym: Sym,
    pub span: Span,
    pub params: IndexMap<Name, (Sym, ParamKind)>,
    pub aliases: IndexMap<Name, Sym>,
}

impl FnSym {
    pub fn new(sym: Sym, span: Span) -> Self {
        Self {
            sym,
            span,
            params: IndexMap::new(),
            aliases: IndexMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructSym {
    pub sym: Sym,
    pub span: Span,
    pub fields: IndexMap<Name, Sym>,
}

impl StructSym {
    pub fn new(sym: Sym, span: Span) -> Self {
        Self {
            sym,
            span,
            fields: IndexMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariantSym {
    pub sym: Sym,
    pub span: Span,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct EnumSym {
    pub sym: Sym,
    pub span: Span,
    pub variants: IndexMap<Name, VariantSym>,
}

impl EnumSym {
    pub fn new(sym: Sym, span: Span) -> Self {
        Self {
            sym,
            span,
            variants: IndexMap::new(),
        }
    }
}
