#[macro_export]
macro_rules! impl_span {
    ($($ty:ty),*) => {
        $(
            impl $ty {
                pub fn span(&self) -> $crate::Span {
                    self.span
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! impl_sym {
    ($($ty:ty),*) => {
        $(
            impl $ty {
                pub fn sym(&self) -> $crate::Sym {
                    self.sym
                }

                pub fn name(&self) -> $crate::Name {
                    self.sym.name
                }
            }
        )*
    };
}
