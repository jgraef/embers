#![allow(dead_code, incomplete_features)]
#![feature(generic_const_exprs)]

pub mod backend;
pub mod element;
pub mod error;
pub mod kernel_new;
pub mod tensor;
mod utils;

pub use crate::{
    backend::gpu::Gpu,
    tensor::Tensor,
};
