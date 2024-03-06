use std::{
    any::{
        type_name,
        TypeId,
    },
    marker::PhantomData,
};

use naga::{
    Expression,
    Handle,
};

use super::{
    error::BuilderError,
    function::FunctionBuilder,
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

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty { .. })
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
        }
    }
}

impl<T: ?Sized> Copy for ExpressionHandle<T> {}

pub trait AsExpression<T: ?Sized> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError>;
}

impl<T> AsExpression<T> for ExpressionHandle<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        Ok(self.clone())
    }
}

pub trait FromExpression<T: ?Sized>: Sized {
    fn from_expression(handle: ExpressionHandle<T>) -> Result<Self, BuilderError>;
}

impl<T: ?Sized> FromExpression<T> for ExpressionHandle<T> {
    fn from_expression(handle: ExpressionHandle<T>) -> Result<Self, BuilderError> {
        Ok(handle)
    }
}

pub trait IntoExpression<T: ?Sized> {
    fn into_expression(self) -> ExpressionHandle<T>;
}

impl<T: ?Sized> IntoExpression<T> for ExpressionHandle<T> {
    fn into_expression(self) -> ExpressionHandle<T> {
        self
    }
}
