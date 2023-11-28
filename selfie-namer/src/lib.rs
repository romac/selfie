//! Namer for the Selfie language

use std::collections::HashSet;

use scope::{EnumSym, FnSym, StructSym};
use selfie_ast::visitor::{ExprVisitorMut, TypeVisitorMut};
use selfie_ast::*;

mod error;
pub use error::Error;

mod symbol_table;
use symbol_table::SymbolTable;

mod scope;
pub use scope::Scope;

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

            self.name_decl_sig(decl);
        }

        for decl in &mut module.decls {
            if let Decl::Fn(fn_decl) = decl {
                self.name_fn_body(fn_decl)
            }
        }
    }

    fn name_fn_body(&mut self, fn_decl: &mut FnDecl) {
        let fn_scope = self.syms.get_fn(&fn_decl.sym.name).unwrap();

        let mut expr_scope = Scope::default();
        for (param, (sym, _)) in &fn_scope.params {
            expr_scope.vars.insert(*param, *sym);
        }
        self.syms.push_scope(expr_scope);

        self.name_expr(&mut fn_decl.body);
    }

    fn name_decl_sig(&mut self, decl: &mut Decl) {
        match decl {
            Decl::Fn(fn_decl) => self.name_fn_decl_sig(fn_decl),
            Decl::Struct(struct_decl) => self.name_struct_decl(struct_decl),
            Decl::Enum(enum_decl) => self.name_enum_decl(enum_decl),
        }
    }

    fn name_fn_decl_sig(&mut self, fn_decl: &mut FnDecl) {
        self.syms.freshen(&mut fn_decl.sym);

        let mut fn_sym = FnSym::new(fn_decl.sym, fn_decl.span);

        for param in &mut fn_decl.params {
            self.syms.freshen(&mut param.sym);

            let span = param.span;

            if let ParamKind::Alias(sym) = &mut param.kind {
                self.syms.freshen(sym);

                if fn_sym.aliases.insert(sym.name, *sym).is_some() {
                    self.errors.push(Error::DuplicateParam(span, *sym));
                }
            }

            if fn_sym
                .params
                .insert(param.sym.name, (param.sym, param.kind))
                .is_some()
            {
                self.errors
                    .push(Error::DuplicateParam(param.span(), param.sym));
            }
        }

        self.syms.add_fn(fn_sym);
    }

    fn name_struct_decl(&mut self, struct_decl: &mut StructDecl) {
        self.syms.freshen(&mut struct_decl.sym);

        let mut struct_sym = StructSym::new(struct_decl.sym, struct_decl.span);
        self.syms.add_struct(struct_sym.clone());

        for field in &mut struct_decl.fields {
            self.syms.freshen(&mut field.sym);

            let duplicate = struct_sym
                .fields
                .insert(field.sym.name, field.sym)
                .is_some();

            if duplicate {
                self.errors
                    .push(Error::DuplicateField(field.span(), field.sym));
            }

            self.name_type(&mut field.ty);
        }

        self.syms.add_struct(struct_sym);
    }

    fn name_enum_decl(&mut self, enum_decl: &mut EnumDecl) {
        self.syms.freshen(&mut enum_decl.sym);

        let mut enum_sym = EnumSym::new(enum_decl.sym, enum_decl.span);
        self.syms.add_enum(enum_sym.clone());

        for variant in &mut enum_decl.variants {
            self.syms.freshen(&mut variant.sym);

            let duplicate = enum_sym
                .variants
                .insert(variant.sym.name, variant.sym)
                .is_some();

            if duplicate {
                self.errors
                    .push(Error::DuplicateVariant(variant.span(), variant.sym));
            }

            if let Some(ty) = &mut variant.ty {
                self.name_type(ty);
            }
        }

        self.syms.add_enum(enum_sym);
    }

    fn name_expr(&mut self, expr: &mut Expr) {
        let mut visitor = NameExprVisitor {
            syms: &mut self.syms,
            errors: &mut self.errors,
        };

        visitor.visit_expr(expr);
    }

    fn name_type(&mut self, ty: &mut Type) {
        let mut visitor = NameTypeVisitor {
            syms: &mut self.syms,
            errors: &mut self.errors,
        };

        visitor.visit_type(ty);
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

impl<'a> ExprVisitorMut for NameExprVisitor<'a> {
    fn visit_var(&mut self, var: &mut Var) {
        if let Some(sym) = self.syms.get_var(&var.sym.name) {
            var.sym = *sym;
        } else {
            self.errors.push(Error::UnboundVar(var.span, var.sym));
        }
    }

    fn visit_fn_call(&mut self, call: &mut FnCall) {
        match self.syms.get_fn(&call.sym.name) {
            None => {
                self.errors.push(Error::UnboundFn(call.span, call.sym));
            }

            Some(fn_sym) => {
                call.sym = fn_sym.sym;

                if call.args.len() != fn_sym.params.len() {
                    self.errors.push(Error::WrongArgCount(
                        call.span,
                        fn_sym.clone(),
                        call.sym,
                        call.args.len(),
                        fn_sym.params.len(),
                    ));

                    return;
                }

                let zipped = call.args.iter_mut().zip(fn_sym.params.values());

                for (arg, (param, kind)) in zipped {
                    match (arg, kind) {
                        (Arg::Named(arg), ParamKind::Anon) => {
                            self.errors.push(Error::ExtraneousArgLabel(
                                arg.span(),
                                fn_sym.clone(),
                                arg.sym,
                                *param,
                            ));
                        }

                        (Arg::Anon(arg), ParamKind::Normal | ParamKind::Alias(_)) => {
                            self.errors.push(Error::MissingArgLabel(
                                arg.span(),
                                fn_sym.clone(),
                                *param,
                            ));
                        }

                        (Arg::Anon(_), ParamKind::Anon) => {
                            // All good
                        }

                        (Arg::Named(named), ParamKind::Normal) => {
                            if named.sym.name == param.name {
                                named.sym = *param;
                            } else {
                                self.errors.push(Error::UnexpectedArg(
                                    named.span(),
                                    fn_sym.clone(),
                                    named.sym,
                                    *param,
                                ));
                            }
                        }

                        (Arg::Named(named), ParamKind::Alias(alias)) => {
                            if named.sym.name == alias.name {
                                // XXX: Should this be the sym of the param or the alias?
                                named.sym = *alias;
                            } else if named.sym.name == param.name {
                                self.errors.push(Error::WrongArgLabel(
                                    named.span(),
                                    fn_sym.clone(),
                                    named.sym,
                                    *alias,
                                ));
                            } else {
                                self.errors.push(Error::UnexpectedArg(
                                    named.span(),
                                    fn_sym.clone(),
                                    named.sym,
                                    *alias,
                                ));
                            }
                        }
                    }
                }
            }
        };

        for arg in &mut call.args {
            match arg {
                Arg::Named(named) => self.visit_expr(&mut named.value),
                Arg::Anon(expr) => self.visit_expr(expr),
            }
        }
    }

    fn visit_method_call(&mut self, _call: &mut MethodCall) {
        todo!()
    }

    fn visit_let(&mut self, let_: &mut Let) {
        self.visit_expr(&mut let_.value);

        self.syms.push_new_scope();

        self.syms.freshen(&mut let_.sym);
        self.syms.add_var(let_.sym);

        self.visit_expr(&mut let_.body);

        self.syms.pop_scope();
    }

    fn visit_field_access(&mut self, field: &mut FieldAccess) {
        self.visit_expr(&mut field.expr);
    }

    fn visit_struct_init(&mut self, init: &mut StructInit) {
        match self.syms.get_struct(&init.sym.name) {
            None => {
                self.errors.push(Error::UnknownType(init.span, init.sym));
            }

            Some(struct_sym) => {
                init.sym = struct_sym.sym;

                for field in struct_sym.fields.keys() {
                    if !init.args.iter().any(|arg| &arg.sym.name == field) {
                        self.errors.push(Error::MissingField(
                            init.span(),
                            struct_sym.clone(),
                            *field,
                        ));
                    }
                }

                for arg in &mut init.args {
                    if let Some(sym) = struct_sym.fields.get(&arg.sym.name) {
                        arg.sym = *sym;
                    } else {
                        self.errors.push(Error::UnknownField(
                            arg.span,
                            struct_sym.clone(),
                            arg.sym,
                        ));
                    }
                }
            }
        }

        for arg in &mut init.args {
            self.visit_expr(&mut arg.value);
        }
    }

    fn visit_enum_init(&mut self, init: &mut EnumInit) {
        if let Some(sym) = init.sym {
            match self.syms.get_enum(&sym.name) {
                None => {
                    self.errors.push(Error::UnknownType(init.span, sym));
                }

                Some(enum_sym) => {
                    init.sym = Some(enum_sym.sym);

                    if let Some(variant_sym) = enum_sym.variants.get(&init.variant.name) {
                        init.variant = *variant_sym;
                    } else {
                        self.errors.push(Error::UnknownVariant(
                            init.span(),
                            enum_sym.clone(),
                            init.variant,
                        ));
                    }
                }
            }
        }

        if let Some(arg) = &mut init.arg {
            self.visit_expr(arg);
        }
    }
}

struct NameTypeVisitor<'a> {
    syms: &'a mut SymbolTable,
    errors: &'a mut Vec<Error>,
}

impl<'a> TypeVisitorMut for NameTypeVisitor<'a> {
    fn visit_named(&mut self, span: Span, sym: &mut Sym) {
        let decl_sym = self
            .syms
            .get_struct(&sym.name)
            .map(|s| s.sym)
            .or_else(|| self.syms.get_enum(&sym.name).map(|s| s.sym));

        if let Some(decl_sym) = decl_sym {
            *sym = decl_sym;
        } else {
            self.errors.push(Error::UnknownType(span, *sym));
        }
    }
}
