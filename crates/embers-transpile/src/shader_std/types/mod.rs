pub mod array;
pub mod atomic;
pub mod scalar;
pub mod vec;

use embers_transpile_macros::transpile;

use crate::{
    builder::{
        error::BuilderError,
        module::ModuleBuilder,
        r#type::{
            TypeHandle,
            Width,
        },
    },
    ShaderType,
};

impl ShaderType for () {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        Ok(module_builder.add_empty_type::<Self>())
    }
}

impl Width for () {
    const WIDTH: usize = 0;
}

#[transpile]
impl crate::shader_std::default::Default for ()
{
    fn default() -> Self {
        ::embers_transpile::__private::intrinsic! {
            ::embers_transpile::__private::ExpressionHandle::<Self>::from_empty()
        }
    }
}