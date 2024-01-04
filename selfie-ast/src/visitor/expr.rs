use crate::*;

pub trait ExprVisitor {
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(expr, self);
    }

    fn visit_lit(&mut self, _lit: &Literal) {}
    fn visit_var(&mut self, _var: &Var) {}

    fn visit_fn_call(&mut self, call: &FnCall) {
        self.visit_args(&call.args);
    }

    fn visit_method_call(&mut self, call: &MethodCall) {
        self.visit_expr(&call.expr);
        self.visit_args(&call.args);
    }

    fn visit_tuple(&mut self, tuple: &Tuple) {
        for expr in &tuple.items {
            self.visit_expr(expr);
        }
    }

    fn visit_match(&mut self, match_: &Match) {
        self.visit_expr(&match_.scrut);

        for case in &match_.cases {
            self.visit_match_case(case);
        }
    }

    fn visit_match_case(&mut self, case: &MatchCase) {
        self.visit_expr(&case.expr);
    }

    fn visit_let(&mut self, let_: &Let) {
        self.visit_expr(&let_.value);
        self.visit_expr(&let_.body);
    }

    fn visit_field_select(&mut self, field: &FieldSelect) {
        self.visit_expr(&field.expr);
    }

    fn visit_tuple_select(&mut self, tuple: &TupleSelect) {
        self.visit_expr(&tuple.expr);
    }

    fn visit_unary_op(&mut self, op: &UnaryOp) {
        self.visit_expr(&op.expr);
    }

    fn visit_binary_op(&mut self, op: &BinaryOp) {
        self.visit_expr(&op.lhs);
        self.visit_expr(&op.rhs);
    }

    fn visit_if(&mut self, if_: &If) {
        self.visit_expr(&if_.cnd);
        self.visit_expr(&if_.thn);
        self.visit_expr(&if_.els);
    }

    fn visit_struct_init(&mut self, init: &StructInit) {
        for field in &init.args {
            self.visit_expr(&field.value);
        }
    }

    fn visit_enum_init(&mut self, init: &EnumInit) {
        if let Some(arg) = &init.arg {
            self.visit_expr(arg);
        }
    }

    fn visit_args(&mut self, args: &[Arg]) {
        for arg in args {
            self.visit_arg(arg);
        }
    }

    fn visit_arg(&mut self, arg: &Arg) {
        match arg {
            Arg::Named(named) => self.visit_expr(&named.value),
            Arg::Anon(expr) => self.visit_expr(expr),
        }
    }
}

pub fn walk_expr<V>(expr: &Expr, visitor: &mut V)
where
    V: ExprVisitor + ?Sized,
{
    match expr {
        Expr::Empty(_) => (),
        Expr::Lit(lit) => visitor.visit_lit(lit),
        Expr::Var(var) => visitor.visit_var(var),
        Expr::FnCall(call) => visitor.visit_fn_call(call),
        Expr::MethodCall(call) => visitor.visit_method_call(call),
        Expr::FieldSelect(field) => visitor.visit_field_select(field),
        Expr::TupleSelect(tuple) => visitor.visit_tuple_select(tuple),
        Expr::Tuple(tuple) => visitor.visit_tuple(tuple),
        Expr::Match(match_) => visitor.visit_match(match_),
        Expr::Let(let_) => visitor.visit_let(let_),
        Expr::UnaryOp(op) => visitor.visit_unary_op(op),
        Expr::BinaryOp(op) => visitor.visit_binary_op(op),
        Expr::If(if_) => visitor.visit_if(if_),
        Expr::StructInit(init) => visitor.visit_struct_init(init),
        Expr::EnumInit(init) => visitor.visit_enum_init(init),
    }
}

pub trait ExprVisitorMut {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr_mut(expr, self);
    }

    fn visit_lit(&mut self, _lit: &mut Literal) {}
    fn visit_var(&mut self, _var: &mut Var) {}

    fn visit_fn_call(&mut self, call: &mut FnCall) {
        self.visit_args(&mut call.args);
    }

    fn visit_method_call(&mut self, call: &mut MethodCall) {
        self.visit_expr(&mut call.expr);
        self.visit_args(&mut call.args);
    }

    fn visit_tuple(&mut self, tuple: &mut Tuple) {
        for expr in &mut tuple.items {
            self.visit_expr(expr);
        }
    }

    fn visit_match(&mut self, match_: &mut Match) {
        self.visit_expr(&mut match_.scrut);

        for case in &mut match_.cases {
            self.visit_match_case(case);
        }
    }

    fn visit_match_case(&mut self, case: &mut MatchCase) {
        self.visit_expr(&mut case.expr);
    }

    fn visit_let(&mut self, let_: &mut Let) {
        self.visit_expr(&mut let_.value);
        self.visit_expr(&mut let_.body);
    }

    fn visit_field_select(&mut self, field: &mut FieldSelect) {
        self.visit_expr(&mut field.expr);
    }

    fn visit_tuple_select(&mut self, tuple: &mut TupleSelect) {
        self.visit_expr(&mut tuple.expr);
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

    fn visit_args(&mut self, args: &mut [Arg]) {
        for arg in args {
            self.visit_arg(arg);
        }
    }

    fn visit_arg(&mut self, arg: &mut Arg) {
        match arg {
            Arg::Named(named) => self.visit_expr(&mut named.value),
            Arg::Anon(expr) => self.visit_expr(expr),
        }
    }
}

pub fn walk_expr_mut<V>(expr: &mut Expr, visitor: &mut V)
where
    V: ExprVisitorMut + ?Sized,
{
    match expr {
        Expr::Empty(_) => (),
        Expr::Lit(lit) => visitor.visit_lit(lit),
        Expr::Var(var) => visitor.visit_var(var),
        Expr::FnCall(call) => visitor.visit_fn_call(call),
        Expr::MethodCall(call) => visitor.visit_method_call(call),
        Expr::FieldSelect(field) => visitor.visit_field_select(field),
        Expr::TupleSelect(tuple) => visitor.visit_tuple_select(tuple),
        Expr::Tuple(tuple) => visitor.visit_tuple(tuple),
        Expr::Match(match_) => visitor.visit_match(match_),
        Expr::Let(let_) => visitor.visit_let(let_),
        Expr::UnaryOp(op) => visitor.visit_unary_op(op),
        Expr::BinaryOp(op) => visitor.visit_binary_op(op),
        Expr::If(if_) => visitor.visit_if(if_),
        Expr::StructInit(init) => visitor.visit_struct_init(init),
        Expr::EnumInit(init) => visitor.visit_enum_init(init),
    }
}
