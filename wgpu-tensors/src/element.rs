use std::{
    borrow::Cow,
    fmt::Debug,
};

use bytemuck::Pod;
use half::f16;

pub trait WgslType {
    const TYPE_NAME: &'static str;

    fn wgsl_literal(&self) -> Cow<'static, str>;
}

impl WgslType for bool {
    const TYPE_NAME: &'static str = "bool";

    fn wgsl_literal(&self) -> Cow<'static, str> {
        self.then_some("true").unwrap_or("false").into()
    }
}

impl WgslType for i32 {
    const TYPE_NAME: &'static str = "i32";

    fn wgsl_literal(&self) -> Cow<'static, str> {
        format!("{}i", self).into()
    }
}

impl WgslType for u32 {
    const TYPE_NAME: &'static str = "u32";

    fn wgsl_literal(&self) -> Cow<'static, str> {
        format!("{}u", self).into()
    }
}

impl WgslType for f32 {
    const TYPE_NAME: &'static str = "f32";

    fn wgsl_literal(&self) -> Cow<'static, str> {
        format!("{}f", self).into()
    }
}

impl WgslType for f16 {
    const TYPE_NAME: &'static str = "f16";

    fn wgsl_literal(&self) -> Cow<'static, str> {
        format!("{}f16", self).into()
    }
}

pub trait EncodeBuffer<T: Encode>: Default {
    fn write(&mut self, value: T) -> Option<T::Encoded>;
    fn flush(self) -> Option<T::Encoded>;
}

/// Trait that defines how elements are encoded when written to or read from a
/// tensor buffer, and how a kernel accesses these encoded elements.
pub trait Encode: Sized {
    type Buffer: EncodeBuffer<Self>;

    /// The type when encoded. This is written into the tensor buffer.
    type Encoded: Pod + WgslType;

    /// How many elements fit into one [`Self::Encoded`] value.
    const NUM_PACKED: usize;

    fn write_into(&self, destination: &mut Self::Encoded, i: usize);

    fn read_from(source: &Self::Encoded, i: usize) -> Self;

    fn encoded_size(num_elements: usize) -> usize {
        num_elements.div_ceil(Self::NUM_PACKED)
    }

    fn buffer_index(index: usize) -> (usize, usize) {
        (index / Self::NUM_PACKED, index % Self::NUM_PACKED)
    }
}

#[derive(Debug, Default)]
pub struct Unbuffered;

impl<T: Encode<Encoded = T>> EncodeBuffer<T> for Unbuffered {
    fn write(&mut self, value: T) -> Option<<T as Encode>::Encoded> {
        Some(value)
    }

    fn flush(self) -> Option<<T as Encode>::Encoded> {
        None
    }
}

pub trait TriviallyEncode: Pod + WgslType {}

impl<T: TriviallyEncode> Encode for T {
    type Buffer = Unbuffered;
    type Encoded = Self;

    const NUM_PACKED: usize = 1;

    fn write_into(&self, buffer: &mut Self::Encoded, i: usize) {
        assert_eq!(i, 0);
        *buffer = *self;
    }

    fn read_from(buffer: &Self::Encoded, i: usize) -> Self {
        assert_eq!(i, 0);
        *buffer
    }
}

impl TriviallyEncode for u32 {}
impl TriviallyEncode for i32 {}
impl TriviallyEncode for f32 {}
impl TriviallyEncode for f16 {}

#[derive(Debug, Default)]
pub struct BoolBuffer {
    buf: u32,
    i: usize,
}

impl EncodeBuffer<bool> for BoolBuffer {
    fn write(&mut self, value: bool) -> Option<<bool as Encode>::Encoded> {
        if value {
            self.buf |= 1 << self.i;
        }
        else {
            self.buf &= !(1 << self.i);
        }

        self.i += 1;
        if self.i == 32 {
            let value = self.buf;
            self.buf = 0;
            Some(value)
        }
        else {
            None
        }
    }

    fn flush(self) -> Option<<bool as Encode>::Encoded> {
        if self.i > 0 {
            Some(self.buf)
        }
        else {
            None
        }
    }
}

impl Encode for bool {
    type Buffer = BoolBuffer;
    type Encoded = u32;

    const NUM_PACKED: usize = 32;

    fn write_into(&self, buffer: &mut Self::Encoded, i: usize) {
        assert!(i < 32);
        if *self {
            *buffer |= 1 << i;
        }
        else {
            *buffer &= !(1 << i);
        }
    }

    fn read_from(buffer: &Self::Encoded, i: usize) -> Self {
        assert!(i < 32);
        *buffer & (1 << i) != 0
    }
}

pub trait Zero {
    const ZERO: Self;
}

pub trait One {
    const ONE: Self;
}

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

impl_zero_and_one!(i32, 0, 1);
impl_zero_and_one!(u32, 0, 1);
impl_zero_and_one!(f32, 0., 1.);
impl_zero_and_one!(f16, f16::from_f32_const(0.), f16::from_f32_const(1.));
impl_zero_and_one!(bool, false, true);

pub trait Element: Copy + Debug + Encode + 'static {}

impl Element for u32 {}
impl Element for i32 {}
impl Element for f32 {}
impl Element for f16 {}
impl Element for bool {}

pub trait Number: Zero + One {}

impl Number for u32 {}
impl Number for i32 {}
impl Number for f32 {}
impl Number for f16 {}
impl Number for bool {}
