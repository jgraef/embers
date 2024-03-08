use std::fmt::Debug;

use naga::Expression;

use crate::builder::{
    block::BlockBuilder,
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
};

#[derive(Clone, Copy, Debug)]
pub struct Literal<T: LiteralValue>(<T as LiteralValue>::Value);

impl<T: LiteralValue> Literal<T> {
    pub fn new(value: <T as LiteralValue>::Value) -> Self {
        Self(value)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum AnyFloat {}

#[derive(Clone, Copy, Debug)]
pub enum AnyInteger {}

mod private {
    use std::fmt::Debug;

    pub trait LiteralValue {
        type Value: Clone + Copy + Debug;
    }
}
use private::LiteralValue;

trait ToNagaLiteral {
    fn to_naga(self) -> naga::Literal;
}

macro_rules! impl_literal_value {
    ($shader_ty:ty, $rust_ty:ty) => {
        impl LiteralValue for $shader_ty {
            type Value = $rust_ty;
        }
    };
}

macro_rules! impl_specific_as_expression {
    ($ty:ty) => {
        impl AsExpression<$ty> for Literal<$ty> {
            fn as_expression(
                &self,
                block_builder: &mut BlockBuilder,
            ) -> Result<ExpressionHandle<$ty>, BuilderError> {
                block_builder
                    .function_builder
                    .add_expression::<$ty>(Expression::Literal(self.0.to_naga()))
            }
        }
    };
}

macro_rules! impl_to_naga_literal {
    ($rust_ty:ty, $variant:ident) => {
        impl ToNagaLiteral for $rust_ty {
            fn to_naga(self) -> naga::Literal {
                naga::Literal::$variant(self)
            }
        }
    };
}

macro_rules! impl_specific_literal {
    ($ty:ident, $variant:ident) => {
        impl_literal_value!(crate::shader_std::types::primitive::$ty, $ty);
        impl_specific_as_expression!(crate::shader_std::types::primitive::$ty);
        impl_to_naga_literal!($ty, $variant);
    };
}

impl_specific_literal!(f64, F64);
impl_specific_literal!(f32, F32);
impl_specific_literal!(u32, U32);
impl_specific_literal!(i32, I32);
//impl_specific_literal!(i64, I64);
impl_specific_literal!(bool, Bool);

macro_rules! impl_generic_as_expression {
    ($any_ty:ty, $to_ty:ident) => {
        impl AsExpression<crate::shader_std::types::primitive::$to_ty> for Literal<$any_ty> {
            fn as_expression(
                &self,
                block_builder: &mut BlockBuilder,
            ) -> Result<ExpressionHandle<crate::shader_std::types::primitive::$to_ty>, BuilderError>
            {
                let x = self.0 as $to_ty;
                block_builder
                    .function_builder
                    .add_expression::<crate::shader_std::types::primitive::$to_ty>(
                        Expression::Literal(x.to_naga()),
                    )
            }
        }
    };
}

impl_literal_value!(AnyInteger, u64);
impl_literal_value!(AnyFloat, f64);
impl_generic_as_expression!(AnyInteger, u32);
impl_generic_as_expression!(AnyInteger, i32);
impl_generic_as_expression!(AnyFloat, f32);
impl_generic_as_expression!(AnyFloat, f64);
