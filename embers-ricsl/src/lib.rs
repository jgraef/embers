#![allow(dead_code, unused_variables)]

mod arena;
pub mod builder;
pub mod ops;
mod types;

#[doc(hidden)]
pub mod __private;

use crate::{
    arena::Handle,
    builder::{
        ModuleBuilder,
        Type,
    },
};

pub trait RicslType: 'static {
    const PHANTOM: bool = false;
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Handle<Type>;
}

#[cfg(feature = "macros")]
pub use embers_ricsl_macros::{
    ricsl,
    RicslType,
};
