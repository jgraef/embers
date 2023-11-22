//#![allow(dead_code, unused_variables)]
#![feature(arbitrary_self_types)]

pub mod builder;
pub mod rstd;

#[doc(hidden)]
pub mod __private;

use crate::builder::{
    ModuleBuilder,
    TypeHandle,
};

pub trait RicslType: 'static {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> TypeHandle;
}

#[cfg(feature = "macros")]
pub use embers_ricsl_macros::{
    ricsl,
    RicslType,
};
