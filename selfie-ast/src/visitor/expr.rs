use crate::*;

pub trait ExprVisitor {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(expr, self);
    }

    fn visit_lit(&mut self, _lit: &mut Literal) {}
    fn visit_var(&mut self, _var: &mut Var) {}

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

    fn visit_tuple(&mut self, tuple: &mut Tuple) {
        for expr in &mut tuple.items {
            self.visit_expr(expr);
        }
    }

    fn visit_return(&mut self, expr: &mut Expr) {
        self.visit_expr(expr);
    }

    fn visit_let(&mut self, let_: &mut Let) {
        self.visit_expr(&mut let_.value);
        self.visit_expr(&mut let_.body);
    }

    fn visit_field_access(&mut self, field: &mut FieldAccess) {
        self.visit_expr(&mut field.expr);
    }

    fn visit_unary_op(&mut self, op: &mut UnaryOp) {
        self.visit_expr(&mut op.expr);
    }

    fn visit_binary_op(&mut self, op: &mut BinaryOp) {
        self.visit_expr(&mut op.lhs);
        self.visit_expr(&mut op.rhs);
    }

    fn visit_if(&mut self, if_: &mut If) {
        self.visit_expr(&mut if_.cnd);
        self.visit_expr(&mut if_.thn);
        self.visit_expr(&mut if_.els);
    }

    fn visit_struct_init(&mut self, init: &mut StructInit) {
        for field in &mut init.args {
            self.visit_expr(&mut field.value);
        }
    }

    fn visit_enum_init(&mut self, init: &mut EnumInit) {
        if let Some(arg) = &mut init.arg {
            self.visit_expr(arg);
        }
    }
}

pub fn walk_expr<V>(expr: &mut Expr, visitor: &mut V)
where
    V: ExprVisitor + ?Sized,
{
    match expr {
        Expr::Lit(lit) => visitor.visit_lit(lit),
        Expr::Var(var) => visitor.visit_var(var),
        Expr::FnCall(call) => visitor.visit_fn_call(call),
        Expr::MethodCall(call) => visitor.visit_method_call(call),
        Expr::FieldAccess(field) => visitor.visit_field_access(field),
        Expr::Tuple(tuple) => visitor.visit_tuple(tuple),
        Expr::Let(let_) => visitor.visit_let(let_),
        Expr::UnaryOp(op) => visitor.visit_unary_op(op),
        Expr::BinaryOp(op) => visitor.visit_binary_op(op),
        Expr::If(if_) => visitor.visit_if(if_),
        Expr::StructInit(init) => visitor.visit_struct_init(init),
        Expr::EnumInit(init) => visitor.visit_enum_init(init),
        Expr::Return(expr) => visitor.visit_return(expr),
    }
}
