use std::{
    any::{
        type_name,
        TypeId,
    },
    marker::PhantomData,
    sync::Arc,
};

use naga::{
    Expression,
    Handle,
    Statement,
};

use super::{
    error::BuilderError,
    function::FunctionBuilder,
    pointer::{
        AddressSpace,
        Pointer,
    },
    r#type::ShaderType,
};

#[derive(Debug)]
#[must_use]
pub enum ExpressionHandle<T: ?Sized> {
    Handle {
        handle: naga::Handle<Expression>,
        _ty: PhantomData<T>,
    },
    Empty {
        _ty: PhantomData<T>,
    },
    Const {
        value: Arc<T>,
    },
}

impl<T> ExpressionHandle<T> {
    pub fn from_constant(value: T) -> Self {
        // if we take an Arc as argument instead, we can even support ?Sized constants
        Self::Const {
            value: Arc::new(value),
        }
    }
}

impl<T: ?Sized> ExpressionHandle<T> {
    pub fn from_handle(handle: Handle<Expression>) -> Self {
        Self::Handle {
            handle,
            _ty: PhantomData,
        }
    }

    pub fn from_empty() -> Self {
        Self::Empty { _ty: PhantomData }
    }

    pub fn get_handle(&self) -> Option<Handle<Expression>> {
        match self {
            ExpressionHandle::Handle { handle, _ty } => Some(*handle),
            ExpressionHandle::Const { .. } => todo!("coerce constant into an expression"),
            _ => None,
        }
    }

    pub fn try_get_handle(&self) -> Result<Handle<Expression>, BuilderError> {
        self.get_handle().ok_or_else(|| {
            BuilderError::NoNagaType {
                ty: type_name::<T>(),
            }
        })
    }

    pub fn get_constant(&self) -> Option<&T> {
        match self {
            ExpressionHandle::Const { value } => Some(value),
            _ => None,
        }
    }
}

impl<T: 'static> ExpressionHandle<T> {
    pub fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

impl<T: ?Sized> Clone for ExpressionHandle<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Handle { handle, _ty } => {
                Self::Handle {
                    handle: *handle,
                    _ty: PhantomData,
                }
            }
            Self::Empty { _ty } => Self::Empty { _ty: PhantomData },
            Self::Const { value } => {
                Self::Const {
                    value: value.clone(),
                }
            }
        }
    }
}

impl<T> AsExpression<T> for ExpressionHandle<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        Ok(self.clone())
    }
}

pub trait AsExpression<T: ?Sized> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError>;
}
