//#![allow(dead_code, unused_variables)]
#![allow(incomplete_features)]
#![feature(arbitrary_self_types, adt_const_params, generic_const_exprs)]

pub mod builder;
pub mod callable;
pub mod shader_std;

// hack to get the proc-macro working from this crate
extern crate self as embers_transpile;

#[doc(hidden)]
pub mod __private;

use std::marker::ConstParamTy;

use crate::builder::{
    ModuleBuilder,
    TypeHandle,
};

#[derive(Debug)]
pub struct Module {
    pub naga: naga::Module,
}

// todo: rename
pub trait RicslType: Sized + 'static {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> TypeHandle;
}

#[derive(ConstParamTy, Copy, Clone, Debug, PartialEq, Eq)]
pub enum FieldAccessor {
    Unnamed(usize),
    Named(&'static str),
}

pub trait FieldAccess<const FIELD: FieldAccessor> {
    const INDEX: usize;
    type Type;
}

pub use embers_transpile_macros::{
    ricsl,
    RicslType,
};
