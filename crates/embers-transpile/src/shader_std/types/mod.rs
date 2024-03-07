pub mod array;
pub mod atomic;
pub mod scalar;
pub mod vec;

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
