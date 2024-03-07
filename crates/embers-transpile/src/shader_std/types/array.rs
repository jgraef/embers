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
            TypeHandle,
            Width,
        },
    },
    shader_std::types::scalar::u32,
    transpile,
    ShaderType,
};

pub struct DynamicArray<T> {
    _ty: PhantomData<T>,
}

#[transpile]
impl<T: ShaderType + Width> DynamicArray<T> {
    pub fn len(&self) -> u32 {
        ::embers_transpile::__private::intrinsic! {
            let expr = crate::__private::AsExpression::as_expression(&_self, function_builder)?.try_get_handle()?;
            function_builder.add_expression::<u32>(crate::__private::naga::Expression::ArrayLength(expr))
        }
    }
}

impl<T: ShaderType + Width> ShaderType for DynamicArray<T> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        let base = module_builder.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data()
        else {
            return Ok(module_builder.add_empty_type::<T>());
        };

        Ok(module_builder.add_naga_type::<Self>(
            None,
            naga::TypeInner::Array {
                base,
                size: naga::ArraySize::Dynamic,
                stride: <T as Width>::WIDTH as _,
            },
        ))
    }
}

pub struct Array<T, const N: usize> {
    handle: ExpressionHandle<Self>,
    _ty: PhantomData<T>,
}

impl<T: ShaderType + Width, const N: usize> ShaderType for Array<T, N> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        if N == 0 {
            return Ok(module_builder.add_empty_type::<T>());
        }
        let n = std::primitive::u32::try_from(N).map_err(|_| BuilderError::Invalid)?;

        let base = module_builder.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data()
        else {
            return Ok(module_builder.add_empty_type::<T>());
        };

        Ok(module_builder.add_naga_type::<Self>(
            None,
            naga::TypeInner::Array {
                base,
                size: naga::ArraySize::Constant(n.try_into().unwrap()),
                stride: <T as Width>::WIDTH as _,
            },
        ))
    }
}

impl<T: ShaderType + Width, const N: usize> AsExpression<Array<T, N>> for Array<T, N> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Self>, BuilderError> {
        Ok(self.handle.clone())
    }
}

impl<T: ShaderType + Width, const N: usize> FromExpression<Array<T, N>> for Array<T, N> {
    fn from_expression(handle: ExpressionHandle<Self>) -> Result<Self, BuilderError> {
        Ok(Self {
            handle,
            _ty: PhantomData,
        })
    }
}

impl<T: ShaderType + Width, const N: usize> IntoExpression<Array<T, N>> for Array<T, N> {
    fn into_expression(self) -> ExpressionHandle<Array<T, N>> {
        self.handle
    }
}

impl<T: ShaderType + Width, const N: usize> Width for Array<T, N> {
    const WIDTH: usize = T::WIDTH * N;
}
