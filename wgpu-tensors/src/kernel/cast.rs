use std::marker::PhantomData;

use half::f16;

use super::map::{
    unary::UnarySignature,
    Map,
};
use crate::{
    element::Element,
    error::KernelError,
    Tensor,
};

pub struct Cast<T, U>(PhantomData<(T, U)>);

macro_rules! cast_kernel {
    ($from:ident, $target:ident) => {
        impl Map for Cast<$from, $target> {
            const LABEL: &'static str = concat!("Cast<", stringify!($target), ">");
            const BODY: &'static str = concat!(
                "result[index_result] = ",
                stringify!($target), /* fixme: this doesn't work if the wgsl name isn't the same
                                      * as the rust name */
                "(operand[index_operand]);"
            );
            type Signature = UnarySignature<$target, $from>;
        }
    };
}

impl<const D: usize, T: Element> Tensor<D, T> {
    pub async fn cast<U: Element>(&self) -> Result<Tensor<D, U>, KernelError>
    where
        Cast<T, U>: Map<Signature = UnarySignature<U, T>>,
    {
        self.map_unary_elementwise::<Cast<T, U>, U>().await
    }
}

// todo: check which casts actually work

cast_kernel!(i32, i32);
cast_kernel!(i32, f16);
cast_kernel!(i32, f32);
//cast_kernel!(i32, bool);

cast_kernel!(f16, i32);
cast_kernel!(f16, f16);
cast_kernel!(f16, f32);
//cast_kernel!(f16, bool);

cast_kernel!(f32, i32);
cast_kernel!(f32, f16);
cast_kernel!(f32, f32);
//cast_kernel!(f32, bool);

//cast_kernel!(bool, i32);
//cast_kernel!(bool, f16);
//cast_kernel!(bool, f32);
//cast_kernel!(bool, bool);
