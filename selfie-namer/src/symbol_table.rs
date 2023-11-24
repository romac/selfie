use selfie_ast::{Decl, Name, Sym};

use crate::scope::{EnumSym, FnSym, StructSym};
use crate::Scope;

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

    pub fn push_new_scope(&mut self) -> &mut Scope {
        self.push_scope(Scope::default())
    }

    pub fn push_scope(&mut self, scope: Scope) -> &mut Scope {
        self.scopes.push(scope);
        self.current_scope_mut()
    }

    pub fn pop_scope(&mut self) -> Scope {
        debug_assert!(!self.scopes.is_empty());

        if self.scopes.len() == 1 {
            panic!("cannot pop root scope");
        }

        self.scopes.pop().expect("we have at least one scope")
    }

    pub fn in_scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.push_new_scope();
        f(self);
        self.pop_scope();
    }

    pub fn current_scope(&self) -> &Scope {
        debug_assert!(!self.scopes.is_empty());

        self.scopes.last().expect("no scopes")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        debug_assert!(!self.scopes.is_empty());

        self.scopes.last_mut().expect("no scopes")
    }

    pub fn add_var(&mut self, sym: Sym) {
        self.current_scope_mut().add_var(sym);
    }

    pub fn get_var(&self, name: &Name) -> Option<&Sym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_var(name))
    }

    pub fn get_var_mut(&mut self, name: &Name) -> Option<&mut Sym> {
        self.current_scope_mut().get_var_mut(name)
    }

    pub fn add_decl(&mut self, decl: &Decl) {
        self.current_scope_mut().add_decl(decl);
    }

    pub fn add_fn(&mut self, sym: Sym) -> &mut FnSym {
        self.current_scope_mut().add_fn(sym)
    }

    pub fn get_fn(&self, name: &Name) -> Option<&FnSym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_fn(name))
    }

    pub fn get_struct(&self, name: &Name) -> Option<&StructSym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_struct(name))
    }

    pub fn get_enum(&self, name: &Name) -> Option<&EnumSym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_enum(name))
    }

    pub fn freshen(&mut self, sym: &mut Sym) {
        sym.id = self.ids.next();
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
