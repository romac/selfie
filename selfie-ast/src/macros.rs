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
