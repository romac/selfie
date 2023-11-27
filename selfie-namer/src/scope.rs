use std::collections::HashMap;

use indexmap::IndexMap;
use selfie_ast::{Name, ParamKind, Sym};

#[derive(Debug)]
pub struct FnSym {
    pub sym: Sym,
    pub params: IndexMap<Name, (Sym, ParamKind)>,
    pub aliases: IndexMap<Name, Sym>,
}

impl FnSym {
    pub fn new(sym: Sym) -> Self {
        Self {
            sym,
            params: IndexMap::new(),
            aliases: IndexMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct StructSym {
    pub sym: Sym,
    pub fields: IndexMap<Name, Sym>,
}

#[derive(Debug)]
pub struct EnumSym {
    pub sym: Sym,
    pub variants: IndexMap<Name, Sym>,
}

#[derive(Default, Debug)]
pub struct Scope {
    pub vars: HashMap<Name, Sym>,
    pub fns: HashMap<Name, FnSym>,
    pub structs: HashMap<Name, StructSym>,
    pub enums: HashMap<Name, EnumSym>,
}

impl Scope {
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

    pub fn add_struct(&mut self, sym: Sym) -> &mut StructSym {
        let struct_sym = StructSym {
            sym,
            fields: IndexMap::new(),
        };

        self.structs.insert(sym.name, struct_sym);
        self.structs.get_mut(&sym.name).unwrap()
    }

    pub fn get_struct(&self, name: &Name) -> Option<&StructSym> {
        self.structs.get(name)
    }

    pub fn get_struct_mut(&mut self, name: &Name) -> Option<&mut StructSym> {
        self.structs.get_mut(name)
    }

    pub fn add_enum(&mut self, sym: Sym) -> &mut EnumSym {
        let enum_sym = EnumSym {
            sym,
            variants: IndexMap::new(),
        };

        self.enums.insert(sym.name, enum_sym);
        self.enums.get_mut(&sym.name).unwrap()
    }

    pub fn get_enum(&self, name: &Name) -> Option<&EnumSym> {
        self.enums.get(name)
    }

    pub fn get_enum_mut(&mut self, name: &Name) -> Option<&mut EnumSym> {
        self.enums.get_mut(name)
    }
}
