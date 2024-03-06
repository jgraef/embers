pub mod array;
pub mod atomic;
pub mod scalar;
pub mod vec;

use crate::{
    builder::{
        error::BuilderError,
        expression::{
            AsExpression,
            ExpressionHandle,
            FromExpression,
            IntoExpression,
        },
        function::FunctionBuilder,
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

impl AsExpression<()> for () {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<()>, BuilderError> {
        Ok(ExpressionHandle::from_empty())
    }
}

impl FromExpression<()> for () {
    fn from_expression(handle: ExpressionHandle<()>) -> Result<Self, BuilderError> {
        assert!(handle.is_empty());
        Ok(())
    }
}

impl IntoExpression<()> for () {
    fn into_expression(self) -> ExpressionHandle<()> {
        ExpressionHandle::from_empty()
    }
}

impl Width for () {
    const WIDTH: usize = 0;
}
