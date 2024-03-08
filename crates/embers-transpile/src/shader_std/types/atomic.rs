use std::marker::PhantomData;

use crate::{
    builder::{
        error::BuilderError,
        module::ModuleBuilder,
        r#type::{
            scalar_to_naga,
            AlignTo,
            ScalarKind,
            TypeHandle,
            Width,
        },
    },
    ShaderType,
};

pub struct Atomic<T> {
    _ty: PhantomData<T>,
}

impl<T: ScalarKind + Width + 'static> ShaderType for Atomic<T> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        Ok(module_builder
            .add_naga_type::<Self>(None, naga::TypeInner::Atomic(scalar_to_naga::<T>())))
    }
}

impl<T: Width> Width for Atomic<T> {
    const WIDTH: u32 = <T as Width>::WIDTH;
}

impl<T: AlignTo> AlignTo for Atomic<T> {
    const ALIGN_TO: u32 = T::ALIGN_TO;
}
