pub use ::naga;
use std::{any::TypeId, marker::PhantomData};
pub use ::std::{
    option::Option,
    result::Result,
    string::String,
    vec,
    vec::Vec,
};

pub use crate::{
    arena::Handle,
    builder::*,
    RicslType,
};


pub fn type_id_from_ref<T: 'static>(value: &T) -> TypeId {
    TypeId::of::<T>()
}

