use std::marker::PhantomData;

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
    shader_std::types::primitive::u32 as shader_u32,
    transpile,
    ShaderType,
};

pub struct DynamicArray<T> {
    _ty: PhantomData<T>,
}

#[transpile]
impl<T: ShaderType + Width> DynamicArray<T> {
    pub fn len(&self) -> shader_u32 {
        ::embers_transpile::__private::intrinsic! {
            let expr = crate::__private::AsExpression::as_expression(&_self, block_builder)?.try_get_naga()?;
            block_builder.function_builder.add_expression::<shader_u32>(crate::__private::naga::Expression::ArrayLength(expr))?
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

impl<T: AlignTo> AlignTo for DynamicArray<T> {
    const ALIGN_TO: u32 = T::ALIGN_TO;
}

pub struct Array<T, const N: u32> {
    _ty: PhantomData<T>,
}

impl<T: ShaderType + Width, const N: u32> ShaderType for Array<T, N> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        if N == 0 {
            return Ok(module_builder.add_empty_type::<T>());
        }

        let base = module_builder.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data()
        else {
            return Ok(module_builder.add_empty_type::<T>());
        };

        Ok(module_builder.add_naga_type::<Self>(
            None,
            naga::TypeInner::Array {
                base,
                size: naga::ArraySize::Constant(N.try_into().unwrap()),
                stride: <T as Width>::WIDTH,
            },
        ))
    }
}

impl<T: Width, const N: u32> Width for Array<T, N> {
    const WIDTH: u32 = T::WIDTH * N;
}

impl<T: AlignTo, const N: u32> AlignTo for Array<T, N> {
    const ALIGN_TO: u32 = T::ALIGN_TO;
}
