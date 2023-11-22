#![allow(dead_code, unused_variables)]
#![feature(arbitrary_self_types)]

pub mod arena;
pub mod builder;
pub mod rstd;

#[doc(hidden)]
pub mod __private;

use crate::{
    arena::Handle,
    builder::{
        ModuleBuilder,
        Type,
    },
};

pub trait RicslType: Sized + 'static {
    const PHANTOM: bool = false;
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Handle<Type>;
}

#[cfg(feature = "macros")]
pub use embers_ricsl_macros::{
    ricsl,
    RicslType,
};
