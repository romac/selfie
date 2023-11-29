use selfie_ast::{Span, Sym};
use thiserror::Error;

use crate::Type;

#[derive(Debug, Error)]
pub enum Error {
    #[error("expected {expected}, found {actual}")]
    TypeMismatch {
        span: Span,
        expected: Type,
        actual: Type,
    },

    #[error("found non-numeric type {actual}")]
    ExpectedNumericType { span: Span, actual: Type },

    #[error("unknown field `{field}`")]
    FieldNotFound { span: Span, ty: Type, field: Sym },

    #[error("cannot infer the type of the enum from the context")]
    AmbiguousEnumVariant { span: Span, variant: Sym },

    #[error("unknown variant `{variant}`")]
    VariantNotFound { span: Span, sym: Sym, variant: Sym },

    #[error("expected tuple type, found: {actual}")]
    ExpectedTupleType { span: Span, actual: Type },

    #[error("index {index} out of bounds for tuple `{ty}`")]
    TupleIndexOutOfBounds { span: Span, index: u16, ty: Type },
}

impl Error {
    pub fn header(&self) -> String {
        match self {
            Error::TypeMismatch { .. } => "type mismatch".to_string(),
            Error::ExpectedNumericType { .. } => "expected numeric type".to_string(),
            Error::FieldNotFound { ty, field, .. } => format!("no field `{field}` on type `{ty}`"),
            Error::VariantNotFound { sym, variant, .. } => {
                format!("no variant `{variant}` on type `{sym}`")
            }
            Error::AmbiguousEnumVariant { variant, .. } => {
                format!("ambiguous enum variant `{variant}`")
            }
            Error::ExpectedTupleType { .. } => {
                "cannot select numeric index on non-tuple".to_string()
            }
            Error::TupleIndexOutOfBounds { .. } => "tuple index out of bounds".to_string(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Error::TypeMismatch { span, .. } => *span,
            Error::ExpectedNumericType { span, .. } => *span,
            Error::FieldNotFound { span, .. } => *span,
            Error::VariantNotFound { span, .. } => *span,
            Error::AmbiguousEnumVariant { span, .. } => *span,
            Error::ExpectedTupleType { span, .. } => *span,
            Error::TupleIndexOutOfBounds { span, .. } => *span,
        }
    }

    pub fn note(&self) -> Option<String> {
        None
    }

    pub fn help(&self) -> Option<String> {
        None
    }
}
