//! Namer for the Selfie language

use std::collections::HashSet;

use thiserror::Error;

use selfie_ast::visitor::{ExprVisitor, TypeVisitor};
use selfie_ast::*;

mod symbol_table;
use symbol_table::SymbolTable;

#[derive(Debug, Error)]
pub enum Error {
    #[error("duplicate module `{1}`")]
    DuplicateModule(Span, Sym),

    #[error("duplicate param `{1}`")]
    DuplicateParam(Span, Sym),

    #[error("duplicate declaration `{1}`")]
    DuplicateDecl(Span, Sym),

    #[error("unbound variable `{1}`")]
    UnboundVar(Span, Sym),
    UnknownType(Sym),
}
impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::DuplicateModule(span, _) => *span,
            Self::DuplicateParam(span, _) => *span,
            Self::DuplicateDecl(span, _) => *span,
            Self::UnboundVar(span, _) => *span,
        }
    }
}

#[derive(Debug)]
pub struct Namer {
    syms: SymbolTable,
    errors: Vec<Error>,
}

impl Namer {
    pub fn new() -> Self {
        Self {
            syms: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    pub fn name_program(mut self, program: &mut Program) -> Result<SymbolTable, Vec<Error>> {
        let mut modules = HashSet::new();

        for module in &mut program.modules {
            if !modules.insert(module.sym.name) {
                return Err(vec![Error::DuplicateModule(module.span(), module.sym)]);
            }

            self.syms.freshen(&mut module.sym);
        }

        for module in &mut program.modules {
            self.name_module(module);
        }

        if self.errors.is_empty() {
            Ok(self.syms)
        } else {
            Err(self.errors)
        }
    }

    fn name_module(&mut self, module: &mut Module) {
        let mut decls = HashSet::new();

        for decl in &mut module.decls {
            if !decls.insert(decl.sym()) {
                self.errors
                    .push(Error::DuplicateDecl(decl.span(), decl.sym()));
            }

            self.syms.freshen(&mut decl.sym());
            self.syms.add_decl(decl);
        }

        for decl in &mut module.decls {
            self.name_decl(decl);
        }
    }

    fn name_decl(&mut self, decl: &mut Decl) {
        match decl {
            Decl::Fn(fn_decl) => self.name_fn_decl(fn_decl),
            Decl::Struct(struct_decl) => self.name_struct_decl(struct_decl),
            Decl::Enum(enum_decl) => self.name_enum_decl(enum_decl),
        }
    }

    fn name_fn_decl(&mut self, fn_decl: &mut FnDecl) {
        self.syms.push_scope();

        let mut params = HashSet::new();
        let mut aliases = HashSet::new();

        for param in &mut fn_decl.params {
            if !params.insert(param.sym) {
                self.errors
                    .push(Error::DuplicateParam(param.span(), param.sym));
            }

            if let ParamKind::Alias(sym) = param.kind {
                if !aliases.insert(sym) {
                    self.errors.push(Error::DuplicateParam(param.span(), sym));
                }
            }

            self.name_param(param);
        }

        self.name_expr(&mut fn_decl.body);

        self.syms.pop_scope();
    }

    fn name_param(&mut self, param: &mut Param) {
        self.syms.add_var(param.sym);
    }

    fn name_expr(&mut self, expr: &mut Expr) {
        let mut visitor = NameExprVisitor {
            syms: &mut self.syms,
            errors: &mut self.errors,
        };

        visitor.visit_expr(expr);
    }

    fn name_struct_decl(&mut self, struct_decl: &mut StructDecl) {
        for field in &mut struct_decl.fields {
            self.syms.freshen(&mut field.sym);
        }
    }

    fn name_enum_decl(&mut self, enum_decl: &mut EnumDecl) {
        for variant in &mut enum_decl.variants {
            self.syms.freshen(&mut variant.sym);
        }
    }
}

impl Default for Namer {
    fn default() -> Self {
        Self::new()
    }
}

struct NameExprVisitor<'a> {
    syms: &'a mut SymbolTable,
    errors: &'a mut Vec<Error>,
}

impl<'a> ExprVisitor for NameExprVisitor<'a> {
    fn visit_var(&mut self, var: &mut Var) {
        if let Some(sym) = self.syms.get_var(&var.sym.name) {
            var.sym = *sym;
        } else {
            self.errors.push(Error::UnboundVar(var.span, var.sym));
        }
    }

    fn visit_fn_call(&mut self, call: &mut FnCall) {
        for arg in &mut call.args {
            match arg {
                Arg::Named(named) => self.visit_expr(&mut named.value),
                Arg::Anon(expr) => self.visit_expr(expr),
            }
        }
    }

    fn visit_method_call(&mut self, call: &mut MethodCall) {
        for arg in &mut call.args {
            match arg {
                Arg::Named(named) => self.visit_expr(&mut named.value),
                Arg::Anon(expr) => self.visit_expr(expr),
            }
        }
    }

    fn visit_let(&mut self, let_: &mut Let) {
        self.visit_expr(&mut let_.value);

        self.syms.push_scope();

        self.syms.freshen(&mut let_.sym);
        self.syms.add_var(let_.sym);

        self.visit_expr(&mut let_.body);

        self.syms.pop_scope();
    }

    fn visit_field_access(&mut self, field: &mut FieldAccess) {
        self.visit_expr(&mut field.expr);
        // TODO: Check that field exists
    }

    fn visit_struct_init(&mut self, init: &mut StructInit) {
        for arg in &mut init.args {
            // TODO: Check that field exist

            self.visit_expr(&mut arg.value);
        }
    }

    fn visit_enum_init(&mut self, init: &mut EnumInit) {
        // TODO: Check that variant exists

        if let Some(arg) = &mut init.arg {
            self.visit_expr(arg);
        }
    }
}

struct NameTypeVisitor<'a> {
    syms: &'a mut SymbolTable,
    errors: &'a mut Vec<Error>,
}

impl<'a> TypeVisitor for NameTypeVisitor<'a> {
    fn visit_named(&mut self, sym: &mut Sym) {
        let decl = self
            .syms
            .get_struct(&sym.name)
            .or_else(|| self.syms.get_enum(&sym.name));

        if let Some(decl) = decl {
            *sym = decl.sym();
        } else {
            self.errors.push(Error::UnknownType(*sym));
        }
    }
}
