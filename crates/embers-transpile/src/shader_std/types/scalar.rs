use crate::{
    builder::r#type::{
        ScalarKind,
        Width,
    },
    transpile,
};

macro_rules! impl_primitive {
    ($ty:ident, $kind:ident, $width:expr) => {
        #[allow(non_camel_case_types)]
        pub struct $ty {
            _nonconstruct: (),
        }

        impl ScalarKind for $ty {
            const KIND: naga::ScalarKind = naga::ScalarKind::$kind;
        }

        impl Width for $ty {
            const WIDTH: usize = $width;
        }
    };
}

macro_rules! impl_unary {
    ($ty:ident, $trait:ident, $method:ident, $op:ident) => {
        #[transpile]
        impl crate::shader_std::ops::$trait for $ty {
            type Output = Self;

            fn $method(self) -> Self::Output {
                ::embers_transpile::__private::intrinsic! {
                    let expr = crate::__private::AsExpression::as_expression(&_self, function_builder)?.try_get_handle()?;
                    function_builder.add_expression::<$ty>(crate::__private::naga::Expression::Unary {
                        op: crate::__private::naga::UnaryOperator::$op,
                        expr,
                    })
                }
            }
        }
    };
}

macro_rules! impl_binary {
    ($ty:ident, $trait:ident, $method:ident, $op:ident) => {
        #[transpile]
        impl crate::shader_std::ops::$trait for $ty {
            type Output = $ty;

            fn $method(self, rhs: $ty) -> Self::Output {
                ::embers_transpile::__private::intrinsic! {
                    let left = crate::__private::AsExpression::as_expression(&_self, function_builder)?.try_get_handle()?;
                    let right = crate::__private::AsExpression::as_expression(&rhs, function_builder)?.try_get_handle()?;
                    function_builder.add_expression::<$ty>(crate::__private::naga::Expression::Binary {
                        op: crate::__private::naga::BinaryOperator::$op,
                        left,
                        right,
                    })
                }
            }
        }
    };
}

macro_rules! impl_scalar {
    ($ty:ident, $kind:ident, $width:expr) => {
        impl_primitive!($ty, $kind, $width);

        impl_unary!($ty, Neg, neg, Negate);

        impl_binary!($ty, Add, add, Add);
        impl_binary!($ty, Sub, sub, Subtract);
        impl_binary!($ty, Mul, mul, Multiply);
        impl_binary!($ty, Div, div, Divide);
        impl_binary!($ty, Rem, rem, Modulo);
    };
}

macro_rules! impl_int {
    ($ty:ident, $kind:ident, $width:expr) => {
        impl_scalar!($ty, $kind, $width);

        impl_unary!($ty, Not, not, BitwiseNot);

        impl_binary!($ty, BitAnd, bitand, And);
        impl_binary!($ty, BitOr, bitor, InclusiveOr);
        impl_binary!($ty, BitXor, bitxor, ExclusiveOr);
        impl_binary!($ty, Shl, shl, ShiftLeft);
        impl_binary!($ty, Shr, shr, ShiftRight);
    };
}

impl_int!(i32, Sint, 4);
impl_int!(u32, Uint, 4);
//#[cfg(feature = "half")]
//impl_scalar!(half::f16, "f16", Float, 2);
impl_scalar!(f32, Float, 4);
impl_scalar!(f64, Float, 8);

impl_primitive!(bool, Bool, 1);
impl_unary!(bool, Not, not, LogicalNot);
impl_binary!(bool, BitAnd, bitand, LogicalAnd);
impl_binary!(bool, BitOr, bitor, LogicalOr);
