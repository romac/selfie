use std::collections::HashMap;

use selfie_ast::{Decl, Name, Sym};

#[derive(Debug)]
pub struct FnSym {
    pub sym: Sym,
    pub params: HashMap<Name, Sym>,
    pub aliases: HashMap<Name, Sym>,
}

#[derive(Debug)]
pub struct StructSym {
    pub sym: Sym,
    pub fields: HashMap<Name, Sym>,
}

#[derive(Debug)]
pub struct EnumSym {
    pub sym: Sym,
    pub variants: HashMap<Name, Sym>,
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

    pub fn add_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Fn(decl) => {
                self.add_fn(decl.sym);
            }
            Decl::Struct(decl) => {
                self.add_struct(decl.sym);
            }
            Decl::Enum(decl) => {
                self.add_enum(decl.sym);
            }
        }
    }

    pub fn add_fn(&mut self, sym: Sym) -> &mut FnSym {
        let fn_sym = FnSym {
            sym,
            params: HashMap::new(),
            aliases: HashMap::new(),
        };

        self.fns.insert(sym.name, fn_sym);
        self.fns.get_mut(&sym.name).unwrap()
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
            fields: HashMap::new(),
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
            variants: HashMap::new(),
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
