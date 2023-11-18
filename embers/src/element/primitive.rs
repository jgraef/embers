use std::borrow::Cow;

use bytemuck::{
    Pod,
    Zeroable,
};
use half::f16;

use super::{
    block::{
        Block,
        DecodeFromBlock,
        EncodeIntoBlock,
    },
    wgsl::{
        WgslDecodeFromBlock,
        WgslEncodeIntoBlock,
        WgslLiteral,
        WgslType,
    },
    Element,
    One,
    Zero,
};

macro_rules! impl_zero_and_one {
    ($ty:ident, $zero:expr, $one:expr) => {
        impl Zero for $ty {
            const ZERO: Self = $zero;
        }
        impl One for $ty {
            const ONE: Self = $one;
        }
    };
}

impl_zero_and_one!(i8, 0, 1);
impl_zero_and_one!(u8, 0, 1);
impl_zero_and_one!(i16, 0, 1);
impl_zero_and_one!(u16, 0, 1);
impl_zero_and_one!(i32, 0, 1);
impl_zero_and_one!(u32, 0, 1);
impl_zero_and_one!(f32, 0., 1.);
impl_zero_and_one!(f16, f16::from_f32_const(0.), f16::from_f32_const(1.));
impl_zero_and_one!(bool, false, true);

impl WgslType for bool {
    const TYPE_NAME: &'static str = "bool";
}

impl WgslLiteral for bool {
    fn format_literal(&self) -> Cow<'static, str> {
        self.then_some("true").unwrap_or("false").into()
    }
}

#[derive(Clone, Copy, Debug, Default, Pod, Zeroable)]
#[repr(transparent)]
pub struct PackedBool(pub u32);

impl WgslType for PackedBool {
    const TYPE_NAME: &'static str = "packed_bool_t";
}

impl Block for PackedBool {
    const NUM_PACKED: usize = 32;
}

impl EncodeIntoBlock<PackedBool> for bool {
    fn encode_into(&self, block: &mut PackedBool, i: usize) {
        assert!(i < 32);
        if *self {
            block.0 |= 1 << i;
        }
        else {
            block.0 &= !(1 << i);
        }
    }
}

impl DecodeFromBlock<PackedBool> for bool {
    fn decode_from(block: &PackedBool, i: usize) -> Self {
        assert!(i < 32);
        block.0 & (1 << i) != 0
    }
}

impl WgslEncodeIntoBlock<PackedBool> for bool {
    const ENCODE: &'static str = r#"
    if value {
        (*output).b |= (1u << i);
    }
    else {
        (*output).b &= ~(1u << i);
    }
    "#;
}

impl WgslDecodeFromBlock<PackedBool> for bool {
    const DECODE: &'static str = "let value = bool(encoded.b & (1u << i));";
}

macro_rules! impl_trivial {
    ($ty:ident, $wgsl_ty:expr, $format:expr) => {
        impl WgslType for $ty {
            const TYPE_NAME: &'static str = $wgsl_ty;
        }

        impl WgslLiteral for $ty {
            fn format_literal(&self) -> Cow<'static, str> {
                format!($format, self).into()
            }
        }

        impl Block for $ty {
            const NUM_PACKED: usize = 1;
        }

        impl EncodeIntoBlock<Self> for $ty {
            fn encode_into(&self, block: &mut Self, _i: usize) {
                *block = *self;
            }
        }

        impl DecodeFromBlock<Self> for $ty {
            fn decode_from(block: &Self, _i: usize) -> Self {
                *block
            }
        }

        impl WgslEncodeIntoBlock<Self> for $ty {
            const ENCODE: &'static str = "*output = value;";
        }

        impl WgslDecodeFromBlock<Self> for $ty {
            const DECODE: &'static str = "let value = encoded;";
        }
    };
}

impl_trivial!(i32, "i32", "{}i");
impl_trivial!(u32, "u32", "{}u");
impl_trivial!(f32, "f32", "{}f");
impl_trivial!(f16, "f16", "{}f16");

macro_rules! impl_packed_int {
    ($ty:ident, $packed:ident, $n:expr, $bits:expr, $mask:expr) => {
        #[derive(Clone, Copy, Debug, Default, Pod, Zeroable)]
        #[repr(transparent)]
        pub struct $packed(pub u32);

        impl Block for $packed {
            const NUM_PACKED: usize = $n;
        }

        impl WgslType for $packed {
            const TYPE_NAME: &'static str = concat!("packed_", stringify!($ty), "_t");
        }

        impl EncodeIntoBlock<$packed> for $ty {
            fn encode_into(&self, block: &mut $packed, i: usize) {
                let buf: &mut [$ty; $n] = bytemuck::cast_mut(block);
                buf[i] = *self;
            }
        }

        impl DecodeFromBlock<$packed> for $ty {
            fn decode_from(block: &$packed, i: usize) -> Self {
                let buf: &[$ty; $n] = bytemuck::cast_ref(block);
                buf[i]
            }
        }

        impl WgslEncodeIntoBlock<$packed> for <$ty as Element>::Primitive {
            const ENCODE: &'static str = concat!(
                "(*output).i = ((value & ",
                stringify!($mask),
                ") << (i * ",
                stringify!($bits),
                ")) | (*output & ~(",
                stringify!($mask),
                " << ",
                stringify!($bits),
                "));"
            );
        }

        impl WgslDecodeFromBlock<$packed> for <$ty as Element>::Primitive {
            const DECODE: &'static str = concat!(
                "let value = (encoded.i >> ",
                stringify!($bits),
                ") & ",
                stringify!($mask),
                ";"
            );
        }
    };
}

impl_packed_int!(i8, PackedI8, 4, 8, 0xFF);
impl_packed_int!(u8, PackedU8, 4, 8, 0xFF);
impl_packed_int!(i16, PackedI16, 2, 16, 0xFFFF);
impl_packed_int!(u16, PackedU16, 2, 16, 0xFFFF);
