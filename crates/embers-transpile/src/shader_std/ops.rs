use embers_transpile_macros::ricsl;

use crate::RicslType;

macro_rules! unary_trait {
    ($name:ident, $method:ident) => {
        #[ricsl]
        pub trait $name: RicslType {
            type Output: RicslType;

            fn $method(self) -> Self::Output;
        }
    };
}

macro_rules! binary_trait {
    ($name:ident, $method:ident) => {
        #[ricsl]
        pub trait $name<Rhs = Self>: RicslType {
            type Output: RicslType;

            fn $method(self, rhs: Rhs) -> Self::Output;
        }
    };
}

macro_rules! binary_assign_trait {
    ($name:ident, $method:ident) => {
        #[ricsl]
        pub trait $name<Rhs = Self>: RicslType {
            fn $method(&mut self, rhs: Rhs);
        }
    };
}

unary_trait!(Neg, neg);
unary_trait!(Not, not);

binary_trait!(Add, add);
binary_trait!(Sub, sub);
binary_trait!(Mul, mul);
binary_trait!(Div, div);
binary_trait!(Rem, rem);
binary_trait!(BitAnd, bitand);
binary_trait!(BitOr, bitor);
binary_trait!(BitXor, bitxor);
binary_trait!(Shl, shl);
binary_trait!(Shr, shr);

binary_assign_trait!(AddAssign, add_assign);
binary_assign_trait!(SubAssign, sub_assign);
binary_assign_trait!(MulAssign, mul_assign);
binary_assign_trait!(DivAssign, div_assign);
binary_assign_trait!(RemAssign, rem_assign);
binary_assign_trait!(BitAndAssign, bitand_assign);
binary_assign_trait!(BitOrAssign, bitor_assign);
binary_assign_trait!(BitXorAssign, bitxor_assign);
binary_assign_trait!(ShlAssign, shl_assign);
binary_assign_trait!(ShrAssign, shr_assign);
