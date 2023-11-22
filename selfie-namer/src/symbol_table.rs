use std::collections::HashMap;

use selfie_ast::{Decl, Name, Sym};

#[derive(Copy, Clone, Default, Debug)]
pub struct Ids {
    next: u32,
}

impl Ids {
    pub fn next(&mut self) -> u32 {
        let id = self.next;
        self.next += 1;
        id
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub ids: Ids,
    pub scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            ids: Ids::default(),
            scopes: vec![Scope::default()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("cannot pop root scope");
        }

        self.scopes.pop();
    }

    pub fn in_scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.push_scope();
        f(self);
        self.pop_scope();
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("no scopes")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("no scopes")
    }

    pub fn add_var(&mut self, sym: Sym) {
        self.current_scope_mut().add_var(sym);
    }

    pub fn get_var(&self, name: &Name) -> Option<&Sym> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get_var(name) {
                return Some(sym);
            }
        }

        None
    }

    pub fn get_var_mut(&mut self, name: &Name) -> Option<&mut Sym> {
        self.current_scope_mut().vars.get_mut(name)
    }

    pub fn add_decl(&mut self, decl: &Decl) {
        self.current_scope_mut().add_decl(decl);
    }

    pub fn get_struct(&self, name: &Name) -> Option<&Sym> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get_struct(name) {
                return Some(sym);
            }
        }

        None
    }

    pub fn get_enum(&self, name: &Name) -> Option<&Sym> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get_enum(name) {
                return Some(sym);
            }
        }

        None
    }

    pub fn freshen(&mut self, sym: &mut Sym) {
        sym.id = self.ids.next();
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    vars: HashMap<Name, Sym>,
    fns: HashMap<Name, Sym>,
    structs: HashMap<Name, Sym>,
    enums: HashMap<Name, Sym>,
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
            Decl::Fn(decl) => self.add_fn(decl.sym),
            Decl::Struct(decl) => self.add_struct(decl.sym),
            Decl::Enum(decl) => self.add_enum(decl.sym),
        }
    }

    pub fn add_fn(&mut self, sym: Sym) {
        self.fns.insert(sym.name, sym);
    }

    pub fn get_fn(&self, name: &Name) -> Option<&Sym> {
        self.fns.get(name)
    }

    pub fn get_fn_mut(&mut self, name: &Name) -> Option<&mut Sym> {
        self.fns.get_mut(name)
    }

    pub fn add_struct(&mut self, sym: Sym) {
        self.structs.insert(sym.name, sym);
    }

    pub fn get_struct(&self, name: &Name) -> Option<&Sym> {
        self.structs.get(name)
    }

    pub fn get_struct_mut(&mut self, name: &Name) -> Option<&mut Sym> {
        self.structs.get_mut(name)
    }

    pub fn add_enum(&mut self, sym: Sym) {
        self.enums.insert(sym.name, sym);
    }

    pub fn get_enum(&self, name: &Name) -> Option<&Sym> {
        self.enums.get(name)
    }

    pub fn get_enum_mut(&mut self, name: &Name) -> Option<&mut Sym> {
        self.enums.get_mut(name)
    }
}
