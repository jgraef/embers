//#![allow(dead_code, unused_variables)]
#![allow(incomplete_features)]
#![feature(arbitrary_self_types, adt_const_params, generic_const_exprs)]

pub mod builder;
pub mod shader_std;
mod utils;

// hack to get the proc-macro working from this crate
extern crate self as embers_transpile;

#[doc(hidden)]
pub mod __private;

pub use embers_transpile_macros::transpile;

pub use crate::builder::module::Module;
