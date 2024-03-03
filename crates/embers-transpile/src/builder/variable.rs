use std::marker::PhantomData;

use naga::{
    Expression,
    LocalVariable,
};

use super::{
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
    function::FunctionBuilder,
    pointer::{
        AddressSpace,
        AsPointer,
        HasAddressSpace,
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

impl<T: ShaderType> AsPointer for LetBinding<T> {
    type Pointer = ExpressionHandle<Pointer<T, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        // todo: create an abstract pointer. we might need to capture the mut-ness of
        // pointers, so we can't actually construct a mut-pointer here
        todo!("LetBinding::into_pointer_expr")
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
    type Pointer = ExpressionHandle<Pointer<T, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        let expr = if let Some(handle) = self.handle {
            function_builder.add_expression(Expression::LocalVariable(handle))
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

impl<T> HasAddressSpace for LetMutBinding<T> {
    const ADDRESS_SPACE: AddressSpace = AddressSpace::Function;
}
