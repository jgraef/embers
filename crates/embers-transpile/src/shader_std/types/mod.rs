pub mod array;
pub mod atomic;
pub mod primitive;
pub mod vec;

use embers_transpile_macros::transpile;

use crate::{
    builder::{
        error::BuilderError,
        module::ModuleBuilder,
        r#type::{
            AlignTo,
            TypeHandle,
            Width,
        },
    },
    ShaderType,
};

pub struct Unit {
    _nonconstruct: (),
}

impl ShaderType for Unit {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        Ok(module_builder.add_empty_type::<Self>())
    }
}

impl Width for Unit {
    const WIDTH: u32 = 0;
}

impl AlignTo for Unit {
    const ALIGN_TO: u32 = 1;
}

#[transpile]
impl crate::shader_std::default::Default for Unit {
    fn default() -> Self {
        ::embers_transpile::__private::intrinsic! {
            ::embers_transpile::__private::ExpressionHandle::<Self>::from_empty()
        }
    }
}
