#![feature(if_let_guard)]

mod error;

pub use error::Error;

mod ctx;
pub use ctx::TyCtx;

mod ty;
pub use ty::Type;

use selfie_ast::symbols::Symbols;
use selfie_ast::*;

pub fn type_program(program: &mut Program, syms: Symbols) -> Result<TyCtx, Vec<Error>> {
    let mut typer = Typer::new(syms);

    for module in &mut program.modules {
        typer.type_module(module)?;
    }

    Ok(typer.ctx)
}

struct Typer {
    ctx: TyCtx,
}

impl Typer {
    pub fn new(syms: Symbols) -> Self {
        Self {
            ctx: TyCtx::new(syms),
        }
    }

    pub fn type_module(&mut self, module: &mut Module) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();

        for decl in &module.decls {
            match decl {
                Decl::Fn(fn_decl) => self.ctx.add_fn_decl(fn_decl.sym, fn_decl),
                Decl::Struct(struct_decl) => self.ctx.add_struct_decl(struct_decl),
                Decl::Enum(enum_decl) => self.ctx.add_enum_decl(enum_decl),
            }
        }

        for decl in &mut module.decls {
            if let Err(err) = self.type_decl(decl) {
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn type_decl(&mut self, decl: &mut Decl) -> Result<(), Error> {
        match decl {
            Decl::Fn(fn_decl) => self.type_fn_decl(fn_decl),
            Decl::Struct(struct_decl) => self.type_struct_decl(struct_decl),
            Decl::Enum(enum_decl) => self.type_enum_decl(enum_decl),
        }
    }

    pub fn type_enum_decl(&mut self, _enum_decl: &EnumDecl) -> Result<(), Error> {
        Ok(())
    }

    pub fn type_struct_decl(&mut self, _struct_decl: &StructDecl) -> Result<(), Error> {
        Ok(())
    }

    pub fn type_fn_decl(&mut self, fn_decl: &mut FnDecl) -> Result<(), Error> {
        for param in &fn_decl.params {
            self.ctx.add_var(param.sym, Type::from(param.ty.clone()));
        }

        self.check_expr(&mut fn_decl.body, &Type::from(&fn_decl.return_type))?;

        Ok(())
    }

    pub fn check_expr(&mut self, mut expr: &mut Expr, expected: &Type) -> Result<(), Error> {
        match (&mut expr, expected) {
            (Expr::Tuple(tup), Type::Tuple(tys)) => {
                self.check_tup(tup, tys)?;
                self.ctx.add_expr(expr, expected.clone());
                return Ok(());
            }

            (Expr::EnumInit(enum_init), Type::Named(sym)) => {
                if enum_init.sym.is_none() {
                    enum_init.sym = Some(*sym);
                    self.check_expr(expr, expected)?;
                }
            }

            (Expr::If(if_), ty) => {
                self.check_expr(&mut if_.cnd, &Type::Bool)?;
                self.check_expr(&mut if_.thn, ty)?;
                self.check_expr(&mut if_.els, ty)?;
            }

            (Expr::Let(let_), ty) => {
                let value_ty = self.infer_expr(&mut let_.value)?;
                self.ctx.add_var(let_.sym, value_ty.clone());
                self.check_expr(&mut let_.body, ty)?;
            }

            _ => (),
        }

        let actual = self.infer_expr(expr)?;

        if &actual != expected {
            return Err(Error::TypeMismatch {
                expected: expected.clone(),
                actual,
                span: expr.span(),
            });
        }

        Ok(())
    }

    pub fn check_tup(&mut self, tup: &mut Tuple, tys: &[Type]) -> Result<(), Error> {
        for (expr, expected) in tup.items.iter_mut().zip(tys) {
            self.check_expr(expr, expected)?;
        }

        Ok(())
    }

    pub fn infer_expr(&mut self, expr: &mut Expr) -> Result<Type, Error> {
        let ty = match expr {
            Expr::Lit(lit) => self.infer_lit(lit),
            Expr::Var(var) => self.infer_var(var),
            Expr::FnCall(fn_call) => self.infer_fn_call(fn_call),
            Expr::MethodCall(_) => todo!(),
            Expr::FieldSelect(field_sel) => self.infer_field_select(field_sel),
            Expr::TupleSelect(tuple_sel) => self.infer_tuple_select(tuple_sel),
            Expr::Tuple(tup) => self.infer_tuple(tup),
            Expr::Let(let_) => self.infer_let(let_),
            Expr::UnaryOp(unop) => self.infer_unop(unop),
            Expr::BinaryOp(binop) => self.infer_binop(binop),
            Expr::If(if_) => self.infer_if(if_),
            Expr::StructInit(struct_init) => self.infer_struct_init(struct_init),

            Expr::EnumInit(enum_init) => self.infer_enum_init(enum_init),
        }?;

        self.ctx.add_expr(expr, ty.clone());

        Ok(ty)
    }

    pub fn infer_var(&self, var: &Var) -> Result<Type, Error> {
        let ty = self.ctx.get_var(var.sym).expect("var not found");
        Ok(ty.clone())
    }

    pub fn infer_lit(&self, lit: &Literal) -> Result<Type, Error> {
        match lit {
            Literal::Int64(_, _) => Ok(Type::Int64),
            Literal::Float64(_, _) => Ok(Type::Float64),
            Literal::Bool(_, _) => Ok(Type::Bool),
            Literal::String(_, _) => Ok(Type::String),
            Literal::Unit(_) => Ok(Type::Unit),
        }
    }

    pub fn infer_fn_call(&mut self, fn_call: &mut FnCall) -> Result<Type, Error> {
        let fn_ty = self.ctx.get_fn(fn_call.sym).cloned().expect("fn not found");

        for (arg, ty) in fn_call.args.iter_mut().zip(&fn_ty.args) {
            self.check_expr(arg.expr_mut(), ty)?;
        }

        Ok(*fn_ty.ret.clone())
    }

    fn infer_tuple_select(&mut self, tuple_sel: &mut TupleSelect) -> Result<Type, Error> {
        let ty = self.infer_expr(&mut tuple_sel.expr)?;

        if let Type::Tuple(tys) = &ty {
            let ty =
                tys.get(tuple_sel.index as usize)
                    .ok_or_else(|| Error::TupleIndexOutOfBounds {
                        span: tuple_sel.span(),
                        index: tuple_sel.index,
                        ty: ty.clone(),
                    })?;

            Ok(ty.clone())
        } else {
            Err(Error::ExpectedTupleType {
                actual: ty,
                span: tuple_sel.span(),
            })
        }
    }

    fn infer_field_select(&mut self, field_sel: &mut FieldSelect) -> Result<Type, Error> {
        let ty = self.infer_expr(&mut field_sel.expr)?;

        match ty {
            Type::Tuple(_tys) => {
                todo!()
            }

            Type::Named(sym) if let Some(struct_decl) = self.ctx.get_struct(sym) => {
                let field_ty = struct_decl
                    .fields
                    .iter()
                    .find(|(f, _)| f.name == field_sel.sym.name);

                if let Some((field, ty)) = field_ty {
                    field_sel.sym = *field;
                    Ok(ty.clone())
                } else {
                    Err(Error::FieldNotFound {
                        ty: Type::Named(sym),
                        field: field_sel.sym,
                        span: field_sel.span(),
                    })
                }
            }

            ty => Err(Error::FieldNotFound {
                ty,
                field: field_sel.sym,
                span: field_sel.span(),
            }),
        }
    }

    pub fn infer_tuple(&mut self, tup: &mut Tuple) -> Result<Type, Error> {
        let tys = tup
            .items
            .iter_mut()
            .map(|expr| self.infer_expr(expr))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Type::Tuple(tys))
    }

    pub fn infer_let(&mut self, let_: &mut Let) -> Result<Type, Error> {
        let ty = self.infer_expr(&mut let_.value)?;
        self.ctx.add_var(let_.sym, ty);
        let ty = self.infer_expr(&mut let_.body)?;
        Ok(ty)
    }

    pub fn infer_unop(&mut self, unop: &mut UnaryOp) -> Result<Type, Error> {
        match unop.op {
            Op1::Not => {
                self.check_expr(&mut unop.expr, &Type::Bool)?;
                Ok(Type::Bool)
            }

            Op1::Neg => {
                let ty = self.infer_expr(&mut unop.expr)?;
                match ty {
                    Type::Int64 => Ok(Type::Int64),
                    Type::Float64 => Ok(Type::Float64),
                    _ => Err(Error::ExpectedNumericType {
                        actual: ty,
                        span: unop.span(),
                    }),
                }
            }
        }
    }

    pub fn infer_binop(&mut self, binop: &mut BinaryOp) -> Result<Type, Error> {
        match binop.op {
            Op2::Add => self.check_same_numeric_type(&mut binop.lhs, &mut binop.rhs),
            Op2::Sub => self.check_same_numeric_type(&mut binop.lhs, &mut binop.rhs),
            Op2::Mul => self.check_same_numeric_type(&mut binop.lhs, &mut binop.rhs),
            Op2::Div => self.check_same_numeric_type(&mut binop.lhs, &mut binop.rhs),
            Op2::Mod => self.check_same_numeric_type(&mut binop.lhs, &mut binop.rhs),

            Op2::Lt => self
                .check_same_numeric_type(&mut binop.lhs, &mut binop.rhs)
                .map(|_| Type::Bool),

            Op2::Gt => self
                .check_same_numeric_type(&mut binop.lhs, &mut binop.rhs)
                .map(|_| Type::Bool),

            Op2::Leq => self
                .check_same_numeric_type(&mut binop.lhs, &mut binop.rhs)
                .map(|_| Type::Bool),

            Op2::Geq => self
                .check_same_numeric_type(&mut binop.lhs, &mut binop.rhs)
                .map(|_| Type::Bool),

            Op2::Eq => self
                .check_same_type(&mut binop.lhs, &mut binop.rhs)
                .map(|_| Type::Bool),

            Op2::Neq => {
                self.check_same_type(&mut binop.lhs, &mut binop.rhs)?;
                Ok(Type::Bool)
            }

            Op2::And => self.check_same_type_exact(&mut binop.lhs, &mut binop.rhs, Type::Bool),
            Op2::Or => self.check_same_type_exact(&mut binop.lhs, &mut binop.rhs, Type::Bool),
        }
    }

    fn check_same_type(&mut self, lhs: &mut Expr, rhs: &mut Expr) -> Result<Type, Error> {
        let lhs = self.infer_expr(lhs)?;
        self.check_expr(rhs, &lhs)?;
        Ok(lhs)
    }

    fn check_same_numeric_type(&mut self, lhs: &mut Expr, rhs: &mut Expr) -> Result<Type, Error> {
        let actual = self.check_same_type(lhs, rhs)?;
        match actual {
            Type::Int64 => Ok(Type::Int64),
            Type::Float64 => Ok(Type::Float64),
            _ => Err(Error::ExpectedNumericType {
                actual,
                span: rhs.span(),
            }),
        }
    }

    fn check_same_type_exact(
        &mut self,
        lhs: &mut Expr,
        rhs: &mut Expr,
        ty: Type,
    ) -> Result<Type, Error> {
        self.check_expr(lhs, &ty)?;
        self.check_expr(rhs, &ty)?;
        Ok(ty)
    }

    pub fn infer_if(&mut self, if_: &mut If) -> Result<Type, Error> {
        self.check_expr(&mut if_.cnd, &Type::Bool)?;
        let ty = self.infer_expr(&mut if_.thn)?;
        self.check_expr(&mut if_.els, &ty)?;
        Ok(ty)
    }

    pub fn infer_struct_init(&mut self, struct_init: &mut StructInit) -> Result<Type, Error> {
        let struct_ty = self
            .ctx
            .get_struct(struct_init.sym)
            .cloned()
            .expect("struct not found");

        for (arg, ty) in struct_init.args.iter_mut().zip(struct_ty.fields.values()) {
            self.check_expr(&mut arg.value, ty)?;
        }

        Ok(Type::Named(struct_init.sym))
    }

    pub fn infer_enum_init(&mut self, enum_init: &mut EnumInit) -> Result<Type, Error> {
        let Some(sym) = enum_init.sym else {
            return Err(Error::AmbiguousEnumVariant {
                span: enum_init.span(),
                variant: enum_init.variant,
            });
        };

        let enum_ty = self.ctx.get_enum(sym).cloned().expect("enum not found");

        let (variant_sym, arg_ty) = enum_ty
            .variants
            .iter()
            .find(|(v, _)| v.name == enum_init.variant.name)
            .ok_or_else(|| Error::VariantNotFound {
                sym,
                variant: enum_init.variant,
                span: enum_init.span(),
            })?;

        enum_init.variant = *variant_sym;

        match (enum_init.arg.as_mut(), arg_ty) {
            (None, None) => Ok(Type::Named(sym)),

            (Some(arg), Some(ty)) => {
                self.check_expr(arg, ty)?;
                Ok(Type::Named(sym))
            }

            _ => unreachable!(),
        }
    }
}
