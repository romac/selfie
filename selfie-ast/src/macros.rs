#[macro_export]
macro_rules! impl_span {
    ($($ty:ty),*) => {
        $(
            impl $ty {
                #[must_use]
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
                #[must_use]
                pub fn sym(&self) -> $crate::Sym {
                    self.sym
                }

                #[must_use]
                pub fn name(&self) -> $crate::Name {
                    self.sym.name
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! impl_attrs {
    ($($ty:ty),*) => {
        $(
            impl $ty {
                #[must_use]
                pub fn attrs(&self) -> &[$crate::Attribute] {
                    &self.attrs
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! impl_decl {
    ($($ty:ty),*) => {
        $(
            $crate::impl_span!($ty);
            $crate::impl_sym!($ty);
            $crate::impl_attrs!($ty);
        )*
    };
}
