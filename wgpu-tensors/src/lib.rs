#![allow(dead_code, incomplete_features)]
#![feature(generic_const_exprs, associated_const_equality)]

pub mod element;
pub mod error;
pub mod gpu;
pub mod kernel;
pub mod tensor;
mod utils;

pub use crate::{tensor::Tensor, gpu::Gpu};
