//! Namer for the Selfie language

#![feature(if_let_guard)]

use std::collections::HashSet;

use selfie_ast::symbols::{EnumSym, FnSym, StructSym, Symbols, VariantSym};
use selfie_ast::visitor::{ExprVisitorMut, TypeVisitorMut};
use selfie_ast::*;

mod error;
pub use error::Error;

mod scope_stack;
use scope_stack::ScopeStack;

#[derive(Default, Debug)]
pub struct Ids {
    next: u32,
}

impl Ids {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> u32 {
        let id = self.next;
        self.next += 1;
        id
    }

    pub fn freshen(&mut self, sym: &mut Sym) {
        sym.id = self.next();
    }
}

pub fn name_program(program: &mut Program) -> Result<Symbols, Vec<Error>> {
    Namer::new()
        .name_program(program)
        .map(|stack| stack.into_global())
}

#[derive(Debug)]
pub struct Namer {
    ids: Ids,
    scope: ScopeStack,
    errors: Vec<Error>,
}

impl Namer {
    pub fn new() -> Self {
        Self {
            ids: Ids::default(),
            scope: ScopeStack::new(),
            errors: Vec::new(),
        }
    }

    pub fn name_program(mut self, program: &mut Program) -> Result<ScopeStack, Vec<Error>> {
        let mut modules = HashSet::new();

        for module in &mut program.modules {
            if !modules.insert(module.sym.name) {
                return Err(vec![Error::DuplicateModule(module.span(), module.sym)]);
            }

            self.ids.freshen(&mut module.sym);
        }

        for module in &mut program.modules {
            self.name_module(module);
        }

        if self.errors.is_empty() {
            Ok(self.scope)
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
                self.name_fn(fn_decl)
            }
        }
    }

    fn name_fn(&mut self, fn_decl: &mut FnDecl) {
        self.name_type(&mut fn_decl.return_type);

        for param in &mut fn_decl.params {
            self.name_type(&mut param.ty);
        }

        let fn_scope = self.scope.get_fn(&fn_decl.sym.name).unwrap();

        let mut expr_scope = Symbols::default();
        for (param, (sym, _)) in &fn_scope.params {
            expr_scope.vars.insert(*param, *sym);
        }
        self.scope.push_scope(expr_scope);

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
        self.ids.freshen(&mut fn_decl.sym);

        let mut fn_sym = FnSym::new(fn_decl.sym, fn_decl.span);

        for param in &mut fn_decl.params {
            self.ids.freshen(&mut param.sym);

            let span = param.span;

            if let ParamKind::Alias(sym) = &mut param.kind {
                self.ids.freshen(sym);

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

        self.scope.add_fn(fn_sym);
    }

    fn name_struct_decl(&mut self, struct_decl: &mut StructDecl) {
        self.ids.freshen(&mut struct_decl.sym);

        let mut struct_sym = StructSym::new(struct_decl.sym, struct_decl.span);
        self.scope.add_struct(struct_sym.clone());

        for field in &mut struct_decl.fields {
            self.ids.freshen(&mut field.sym);

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

        self.scope.add_struct(struct_sym);
    }

    fn name_enum_decl(&mut self, enum_decl: &mut EnumDecl) {
        self.ids.freshen(&mut enum_decl.sym);

        let mut enum_sym = EnumSym::new(enum_decl.sym, enum_decl.span);
        self.scope.add_enum(enum_sym.clone());

        for variant in &mut enum_decl.variants {
            self.ids.freshen(&mut variant.sym);

            let variant_sym = VariantSym {
                sym: variant.sym,
                span: variant.span,
                ty: variant.ty.clone(),
            };

            let duplicate = enum_sym
                .variants
                .insert(variant.sym.name, variant_sym)
                .is_some();

            if duplicate {
                self.errors
                    .push(Error::DuplicateVariant(variant.span(), variant.sym));
            }

            if let Some(ty) = &mut variant.ty {
                self.name_type(ty);
            }
        }

        self.scope.add_enum(enum_sym);
    }

    fn name_expr(&mut self, expr: &mut Expr) {
        let mut visitor = NameExprVisitor {
            ids: &mut self.ids,
            scope: &mut self.scope,
            errors: &mut self.errors,
        };

        visitor.visit_expr(expr);
    }

    fn name_type(&mut self, ty: &mut Type) {
        let mut visitor = NameTypeVisitor {
            scope: &mut self.scope,
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
    ids: &'a mut Ids,
    scope: &'a mut ScopeStack,
    errors: &'a mut Vec<Error>,
}

impl<'a> ExprVisitorMut for NameExprVisitor<'a> {
    fn visit_var(&mut self, var: &mut Var) {
        if let Some(sym) = self.scope.get_var(&var.sym.name) {
            var.sym = *sym;
        } else {
            self.errors.push(Error::UnboundVar(var.span, var.sym));
        }
    }

    fn visit_fn_call(&mut self, call: &mut FnCall) {
        match self.scope.get_fn(&call.sym.name) {
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

    fn visit_match_case(&mut self, case: &mut MatchCase) {
        self.scope.push_new_scope();

        fn visit_pattern(
            pattern: &mut Pattern,
            ids: &mut Ids,
            scope: &mut ScopeStack,
            errors: &mut Vec<Error>,
        ) {
            match pattern {
                Pattern::Wildcard(_) => (),

                Pattern::Var(v) => {
                    ids.freshen(&mut v.sym);
                    scope.add_var(v.sym);
                }

                Pattern::Tuple(tpat) => {
                    for pat in &mut tpat.items {
                        visit_pattern(pat, ids, scope, errors);
                    }
                }

                Pattern::Enum(epat) => {
                    if let Some(sym) = epat.ty {
                        let Some(enum_sym) = scope.get_enum(&sym.name) else {
                            errors.push(Error::UnknownType(epat.span, sym));
                            return;
                        };

                        epat.ty = Some(enum_sym.sym);
                    }

                    // TODO: Check that variant arg matches definition

                    if let Some(evar) = &mut epat.arg {
                        visit_pattern(evar, ids, scope, errors);
                    }
                }
            }
        }

        visit_pattern(&mut case.pattern, self.ids, self.scope, self.errors);

        self.visit_expr(&mut case.expr);

        self.scope.pop_scope();
    }

    fn visit_let(&mut self, let_: &mut Let) {
        self.visit_expr(&mut let_.value);

        self.scope.push_new_scope();

        self.ids.freshen(&mut let_.sym);
        self.scope.add_var(let_.sym);

        self.visit_expr(&mut let_.body);

        self.scope.pop_scope();
    }

    fn visit_field_select(&mut self, field: &mut FieldSelect) {
        self.visit_expr(&mut field.expr);
    }

    fn visit_struct_init(&mut self, init: &mut StructInit) {
        match self.scope.get_struct(&init.sym.name) {
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
        let Some(sym) = init.ty else {
            init.arg
                .iter_mut()
                .for_each(|arg| self.visit_expr(arg.as_mut()));

            return;
        };

        let Some(enum_sym) = self.scope.get_enum(&sym.name) else {
            self.errors.push(Error::UnknownType(init.span, sym));
            return;
        };

        init.ty = Some(enum_sym.sym);

        if let Some(variant_sym) = enum_sym.variants.get(&init.variant.name) {
            init.variant = variant_sym.sym;

            match (&mut init.arg, &variant_sym.ty) {
                (Some(_), None) => {
                    self.errors.push(Error::UnexpectedVariantArg(
                        init.span(),
                        enum_sym.clone(),
                        init.variant,
                    ));
                }

                (None, Some(_)) => {
                    self.errors.push(Error::MissingVariantArg(
                        init.span(),
                        enum_sym.clone(),
                        init.variant,
                    ));
                }

                (Some(arg), Some(_)) => {
                    self.visit_expr(arg.as_mut());
                }

                (None, None) => {}
            }
        } else {
            self.errors.push(Error::UnknownVariant(
                init.span(),
                enum_sym.clone(),
                init.variant,
            ));
        }
    }
}

struct NameTypeVisitor<'a> {
    scope: &'a mut ScopeStack,
    errors: &'a mut Vec<Error>,
}

impl<'a> TypeVisitorMut for NameTypeVisitor<'a> {
    fn visit_named(&mut self, span: Span, sym: &mut Sym) {
        let decl_sym = self
            .scope
            .get_struct(&sym.name)
            .map(|s| s.sym)
            .or_else(|| self.scope.get_enum(&sym.name).map(|s| s.sym));

        if let Some(decl_sym) = decl_sym {
            *sym = decl_sym;
        } else {
            self.errors.push(Error::UnknownType(span, *sym));
        }
    }
}
