use std::{
    any::{
        type_name,
        TypeId,
    },
    marker::PhantomData,
};

use naga::{
    Block,
    Expression,
    Handle,
};

use super::{
    block::BlockBuilder,
    error::BuilderError,
    function::{
        Argument,
        DynFnInputBinding,
    },
    pointer::AddressSpace,
    variable::GlobalVariable,
};

#[derive(Clone, Copy, Debug)]
pub enum DynExpressionHandle {
    Handle {
        handle: Handle<Expression>,
        is_const: bool,
    },
    Empty,
}

impl DynExpressionHandle {
    pub fn from_typed<T: ?Sized>(handle: ExpressionHandle<T>) -> Self {
        handle.dyn_handle
    }

    pub fn from_naga(handle: Handle<Expression>) -> Self {
        Self::Handle {
            handle,
            is_const: false,
        }
    }

    pub fn empty() -> Self {
        Self::Empty
    }

    pub fn get_naga(&self) -> Option<Handle<Expression>> {
        match self {
            DynExpressionHandle::Handle { handle, .. } => Some(*handle),
            DynExpressionHandle::Empty => None,
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    pub fn promote_const(&mut self) {
        match self {
            Self::Handle { is_const, .. } => *is_const = true,
            Self::Empty => {}
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Self::Handle { is_const, .. } => *is_const,
            Self::Empty => true,
        }
    }
}

#[derive(Debug)]
pub struct ExpressionHandle<T: ?Sized> {
    dyn_handle: DynExpressionHandle,
    _type: PhantomData<T>,
}

impl<T: ?Sized> ExpressionHandle<T> {
    pub fn from_dyn(dyn_handle: DynExpressionHandle) -> Self {
        Self {
            dyn_handle,
            _type: PhantomData,
        }
    }

    pub fn from_naga(handle: Handle<Expression>) -> Self {
        Self::from_dyn(DynExpressionHandle::from_naga(handle))
    }

    pub fn from_arg(
        _: Argument<T>,
        input: DynFnInputBinding,
        block_builder: &mut BlockBuilder,
    ) -> Result<Self, BuilderError> {
        let expr = match input {
            DynFnInputBinding::Input { index } => {
                if let Some(index) = index {
                    block_builder
                        .function_builder
                        .add_expression(Expression::FunctionArgument(index as u32))?
                }
                else {
                    ExpressionHandle::empty()
                }
            }
            DynFnInputBinding::Expression { dyn_handle } => ExpressionHandle::from_dyn(dyn_handle),
        };

        Ok(expr)
    }

    pub fn empty() -> Self {
        Self::from_dyn(DynExpressionHandle::empty())
    }

    pub fn get_naga(&self) -> Option<Handle<Expression>> {
        self.dyn_handle.get_naga()
    }

    pub fn try_get_naga(&self) -> Result<Handle<Expression>, BuilderError> {
        self.get_naga().ok_or_else(|| {
            BuilderError::NoNagaType {
                ty: type_name::<T>(),
            }
        })
    }

    pub fn is_empty(&self) -> bool {
        self.dyn_handle.is_empty()
    }

    pub fn promote_const(&mut self) {
        self.dyn_handle.promote_const()
    }

    pub fn is_const(&self) -> bool {
        self.dyn_handle.is_const()
    }

    pub fn as_dyn(&self) -> DynExpressionHandle {
        self.dyn_handle
    }

    pub fn as_dyn_input(&self) -> DynFnInputBinding {
        DynFnInputBinding::Expression {
            dyn_handle: self.dyn_handle,
        }
    }
}

impl<T: ?Sized + 'static> ExpressionHandle<T> {
    pub fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

impl<T: ?Sized> Clone for ExpressionHandle<T> {
    fn clone(&self) -> Self {
        Self {
            dyn_handle: self.dyn_handle.clone(),
            _type: PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for ExpressionHandle<T> {}

pub trait AsExpression<T: ?Sized> {
    fn as_expression(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError>;
}

impl<T: ?Sized> AsExpression<T> for ExpressionHandle<T> {
    fn as_expression(
        &self,
        _block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        Ok(self.clone())
    }
}
