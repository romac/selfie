use crate::*;

pub trait TypeVisitor {
    fn visit_type(&mut self, ty: &Type) {
        walk_type(ty, self)
    }

    fn visit_int64(&mut self, _span: Span) {}
    fn visit_float64(&mut self, _span: Span) {}
    fn visit_string(&mut self, _span: Span) {}
    fn visit_bool(&mut self, _span: Span) {}
    fn visit_unit(&mut self, _span: Span) {}

    fn visit_tuple(&mut self, _span: Span, tys: &[Type]) {
        for ty in tys {
            self.visit_type(ty);
        }
    }

    fn visit_named(&mut self, _span: Span, _sym: &Sym) {}

    fn visit_fn(&mut self, _span: Span, args: &[Type], ret: &Type) {
        for arg in args {
            self.visit_type(arg);
        }

        self.visit_type(ret);
    }
}

pub fn walk_type<V>(ty: &Type, visitor: &mut V)
where
    V: TypeVisitor + ?Sized,
{
    match ty {
        Type::Int64(span) => visitor.visit_int64(*span),
        Type::Float64(span) => visitor.visit_float64(*span),
        Type::String(span) => visitor.visit_string(*span),
        Type::Bool(span) => visitor.visit_bool(*span),
        Type::Unit(span) => visitor.visit_unit(*span),
        Type::Tuple(span, tys) => visitor.visit_tuple(*span, tys),
        Type::Named(span, sym) => visitor.visit_named(*span, sym),
        Type::Fn { span, args, ret } => visitor.visit_fn(*span, args, ret),
    }
}

pub trait TypeVisitorMut {
    fn visit_type(&mut self, ty: &mut Type) {
        walk_type_mut(ty, self)
    }

    fn visit_int64(&mut self, _span: Span) {}
    fn visit_float64(&mut self, _span: Span) {}
    fn visit_string(&mut self, _span: Span) {}
    fn visit_bool(&mut self, _span: Span) {}
    fn visit_unit(&mut self, _span: Span) {}

    fn visit_tuple(&mut self, _span: Span, tys: &mut [Type]) {
        for ty in tys {
            self.visit_type(ty);
        }
    }

    fn visit_named(&mut self, _span: Span, _sym: &mut Sym) {}

    fn visit_fn(&mut self, _span: Span, args: &mut [Type], ret: &mut Type) {
        for arg in args {
            self.visit_type(arg);
        }

        self.visit_type(ret);
    }
}

pub fn walk_type_mut<V>(ty: &mut Type, visitor: &mut V)
where
    V: TypeVisitorMut + ?Sized,
{
    match ty {
        Type::Int64(span) => visitor.visit_int64(*span),
        Type::Float64(span) => visitor.visit_float64(*span),
        Type::String(span) => visitor.visit_string(*span),
        Type::Bool(span) => visitor.visit_bool(*span),
        Type::Unit(span) => visitor.visit_unit(*span),
        Type::Tuple(span, tys) => visitor.visit_tuple(*span, tys),
        Type::Named(span, sym) => visitor.visit_named(*span, sym),
        Type::Fn { span, args, ret } => visitor.visit_fn(*span, args, ret),
    }
}
