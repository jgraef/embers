use embers_transpile_macros::transpile;

use crate::builder::{
    error::BuilderError,
    module::ModuleBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
        Width,
    },
};

pub struct PhantomData<T> {
    _t: std::marker::PhantomData<T>,
}

impl<T: 'static> ShaderType for PhantomData<T> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        Ok(module_builder.add_empty_type::<Self>())
    }
}

impl<T> Width for PhantomData<T> {
    const WIDTH: usize = 0;
}

#[transpile]
impl<T: 'static> crate::shader_std::default::Default for PhantomData<T> {
    fn default() -> Self {
        ::embers_transpile::__private::intrinsic! {
            ::embers_transpile::__private::ExpressionHandle::<Self>::from_empty()
        }
    }
}
