use crate::*;

pub trait TypeVisitor {
    fn visit_type(&mut self, ty: &Type) {
        walk_type(ty, self)
    }

    fn visit_int64(&mut self) {}
    fn visit_float64(&mut self) {}
    fn visit_string(&mut self) {}
    fn visit_bool(&mut self) {}
    fn visit_unit(&mut self) {}

    fn visit_tuple(&mut self, tys: &[Type]) {
        for ty in tys {
            self.visit_type(ty);
        }
    }

    fn visit_named(&mut self, _sym: &Sym) {}

    fn visit_fn(&mut self, args: &[Type], ret: &Type) {
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
        Type::Int64 => visitor.visit_int64(),
        Type::Float64 => visitor.visit_float64(),
        Type::String => visitor.visit_string(),
        Type::Bool => visitor.visit_bool(),
        Type::Unit => visitor.visit_unit(),
        Type::Tuple(tys) => visitor.visit_tuple(tys),
        Type::Named(sym) => visitor.visit_named(sym),
        Type::Fn { args, ret } => visitor.visit_fn(args, ret),
    }
}

pub trait TypeVisitorMut {
    fn visit_type(&mut self, ty: &mut Type) {
        walk_type_mut(ty, self)
    }

    fn visit_int64(&mut self) {}
    fn visit_float64(&mut self) {}
    fn visit_string(&mut self) {}
    fn visit_bool(&mut self) {}
    fn visit_unit(&mut self) {}

    fn visit_tuple(&mut self, tys: &mut [Type]) {
        for ty in tys {
            self.visit_type(ty);
        }
    }

    fn visit_named(&mut self, _sym: &mut Sym) {}

    fn visit_fn(&mut self, args: &mut [Type], ret: &mut Type) {
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
        Type::Int64 => visitor.visit_int64(),
        Type::Float64 => visitor.visit_float64(),
        Type::String => visitor.visit_string(),
        Type::Bool => visitor.visit_bool(),
        Type::Unit => visitor.visit_unit(),
        Type::Tuple(tys) => visitor.visit_tuple(tys),
        Type::Named(sym) => visitor.visit_named(sym),
        Type::Fn { args, ret } => visitor.visit_fn(args, ret),
    }
}
