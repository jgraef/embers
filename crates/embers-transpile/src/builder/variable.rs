use std::marker::PhantomData;

use naga::{
    Expression,
    Handle,
    LocalVariable,
    ResourceBinding,
};

use super::{
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
    function::FunctionBuilder,
    pointer::{
        address_space,
        AddressSpace,
        AsPointer,
        Pointer,
    },
    r#type::ShaderType,
};

pub trait Assign<T> {
    fn assign<E: AsExpression<T>>(
        &self,
        value: E,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError>;
}

/// A non-mutable let binding, represented by a named expression in naga, and
/// optionally initialized with an expression.
pub struct LetBinding<T> {
    value: Option<ExpressionHandle<T>>,
}

impl<T> LetBinding<T> {
    pub fn unbound() -> Self {
        Self { value: None }
    }

    pub fn from_expr(expr: ExpressionHandle<T>) -> Self {
        Self { value: Some(expr) }
    }
}

impl<T> AsExpression<T> for LetBinding<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        self.value.clone().ok_or(BuilderError::LetUnbound)
    }
}

/// A mutable let binding, represented by a local variable in naga.
pub struct LetMutBinding<T> {
    handle: Option<naga::Handle<LocalVariable>>,
    _ty: PhantomData<T>,
}

impl<T> LetMutBinding<T> {
    pub fn from_handle(handle: naga::Handle<LocalVariable>) -> Self {
        Self {
            handle: Some(handle),
            _ty: PhantomData,
        }
    }

    pub fn from_empty() -> Self {
        Self {
            handle: None,
            _ty: PhantomData,
        }
    }
}

impl<T: ShaderType> AsExpression<T> for LetMutBinding<T> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let pointer = self.as_pointer(function_builder)?;
        let expr = pointer.load(function_builder)?;

        Ok(expr)
    }
}

impl<T: ShaderType> AsPointer for LetMutBinding<T> {
    type Base = T;
    type AddressSpace = address_space::Function;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, Self::AddressSpace>>, BuilderError> {
        let expr = if let Some(handle) = self.handle {
            function_builder.add_expression(Expression::LocalVariable(handle))?
        }
        else {
            ExpressionHandle::from_empty()
        };

        Ok(expr)
    }
}

impl<T: ShaderType> Assign<T> for LetMutBinding<T> {
    fn assign<E: AsExpression<T>>(
        &self,
        value: E,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError> {
        let value = value.as_expression(function_builder)?;
        let pointer = self.as_pointer(function_builder)?;
        pointer.store(&value, function_builder)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum GlobalVariableHandle {
    Handle(naga::Handle<naga::GlobalVariable>),
    Empty,
}

#[derive(Clone, Copy, Debug)]
pub struct GlobalVariable<T: ?Sized, A> {
    handle: GlobalVariableHandle,
    _ty: PhantomData<T>,
    _address_space: PhantomData<A>,
}

impl<T: ?Sized, A> GlobalVariable<T, A> {
    pub fn new(handle: GlobalVariableHandle) -> Self {
        Self {
            handle,
            _ty: PhantomData,
            _address_space: PhantomData,
        }
    }

    pub fn from_handle(handle: naga::Handle<naga::GlobalVariable>) -> Self {
        Self {
            handle: GlobalVariableHandle::Handle(handle),
            _ty: PhantomData,
            _address_space: PhantomData,
        }
    }

    pub fn from_empty() -> Self {
        Self {
            handle: GlobalVariableHandle::Empty,
            _ty: PhantomData,
            _address_space: PhantomData,
        }
    }
}

impl<T: ShaderType + ?Sized, A: AddressSpace> AsExpression<Pointer<T, A>> for GlobalVariable<T, A> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError> {
        let expression_handle = match &self.handle {
            GlobalVariableHandle::Handle(handle) => {
                function_builder.add_expression(Expression::GlobalVariable(*handle))?
            }
            GlobalVariableHandle::Empty => ExpressionHandle::from_empty(),
        };
        Ok(expression_handle)
    }
}
