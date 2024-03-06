use std::marker::PhantomData;

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
            scalar_to_naga,
            Scalar,
            TypeHandle,
            Width,
        },
    },
    ShaderType,
};

pub struct Atomic<T> {
    handle: ExpressionHandle<Self>,
    _ty: PhantomData<T>,
}

impl<T: Scalar + Width + 'static> ShaderType for Atomic<T> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        Ok(module_builder
            .add_naga_type::<Self>(None, naga::TypeInner::Atomic(scalar_to_naga::<T>())))
    }
}

impl<T> AsExpression<Atomic<T>> for Atomic<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Self>, BuilderError> {
        Ok(self.handle.clone())
    }
}

impl<T> FromExpression<Atomic<T>> for Atomic<T> {
    fn from_expression(handle: ExpressionHandle<Self>) -> Result<Self, BuilderError> {
        Ok(Self {
            handle,
            _ty: PhantomData,
        })
    }
}

impl<T> IntoExpression<Atomic<T>> for Atomic<T> {
    fn into_expression(self) -> ExpressionHandle<Atomic<T>> {
        self.handle
    }
}

impl<T: Width> Width for Atomic<T> {
    const WIDTH: usize = <T as Width>::WIDTH;
}
