use selfie_ast::{Name, Sym};

use selfie_ast::symbols::{EnumSym, FnSym, StructSym, Symbols};

#[derive(Debug)]
pub struct ScopeStack {
    scopes: Vec<Symbols>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![Symbols::default()],
        }
    }

    pub fn push_new_scope(&mut self) -> &mut Symbols {
        self.push_scope(Symbols::default())
    }

    pub fn push_scope(&mut self, scope: Symbols) -> &mut Symbols {
        self.scopes.push(scope);
        self.current_scope_mut()
    }

    pub fn pop_scope(&mut self) -> Symbols {
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

    pub fn current_scope(&self) -> &Symbols {
        debug_assert!(!self.scopes.is_empty());

        self.scopes.last().expect("no scopes")
    }

    pub fn current_scope_mut(&mut self) -> &mut Symbols {
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

    pub fn add_fn(&mut self, fn_sym: FnSym) {
        self.current_scope_mut().add_fn(fn_sym)
    }

    pub fn get_fn(&self, name: &Name) -> Option<&FnSym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_fn(name))
    }

    pub fn add_struct(&mut self, struct_sym: StructSym) {
        self.current_scope_mut().add_struct(struct_sym)
    }

    pub fn get_struct(&self, name: &Name) -> Option<&StructSym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_struct(name))
    }

    pub fn add_enum(&mut self, enum_sym: EnumSym) {
        self.current_scope_mut().add_enum(enum_sym)
    }

    pub fn get_enum(&self, name: &Name) -> Option<&EnumSym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_enum(name))
    }

    pub fn into_global(mut self) -> Symbols {
        self.scopes.swap_remove(0)
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}
