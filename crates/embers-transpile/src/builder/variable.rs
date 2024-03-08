use std::marker::PhantomData;

use naga::{
    Expression,
    LocalVariable,
};

use super::{
    block::BlockBuilder,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScopeId(pub(super) usize);

pub trait Assign<T> {
    fn assign<E: AsExpression<T>>(
        &self,
        value: E,
        block_builder: &mut BlockBuilder,
    ) -> Result<(), BuilderError>;
}

/// A non-mutable let binding, represented by a named expression in naga, and
/// optionally initialized with an expression.
pub struct LetBinding<T> {
    value: Option<ExpressionHandle<T>>,
    scope: ScopeId,
}

impl<T> LetBinding<T> {
    pub fn unbound(scope: ScopeId) -> Self {
        Self { value: None, scope }
    }

    pub fn from_expr(expr: ExpressionHandle<T>, scope: ScopeId) -> Self {
        Self { value: Some(expr), scope }
    }
}

impl<T> AsExpression<T> for LetBinding<T> {
    fn as_expression(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let value = self.value.clone().ok_or(BuilderError::LetUnbound)?;
        //let value = block_builder.function_builder.capture(self.scope, value)?;
        Ok(value)
    }
}

/// A mutable let binding, represented by a local variable in naga.
pub struct LetMutBinding<T> {
    handle: Option<naga::Handle<LocalVariable>>,
    scope: ScopeId,
    _ty: PhantomData<T>,
}

impl<T> LetMutBinding<T> {
    pub fn new(handle: naga::Handle<LocalVariable>, scope: ScopeId) -> Self {
        Self {
            handle: Some(handle),
            scope,
            _ty: PhantomData,
        }
    }

    pub fn empty(scope: ScopeId) -> Self {
        Self {
            handle: None,
            scope,
            _ty: PhantomData,
        }
    }
}

impl<T: ShaderType> AsExpression<T> for LetMutBinding<T> {
    fn as_expression(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let pointer = self.as_pointer(&mut block_builder.function_builder)?;
        let expr = pointer.load(block_builder)?;

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
            // todo: capture
            function_builder.add_expression(Expression::LocalVariable(handle))?
        }
        else {
            ExpressionHandle::empty()
        };

        Ok(expr)
    }
}

impl<T: ShaderType> Assign<T> for LetMutBinding<T> {
    fn assign<E: AsExpression<T>>(
        &self,
        value: E,
        mut block_builder: &mut BlockBuilder,
    ) -> Result<(), BuilderError> {
        let value = value.as_expression(&mut block_builder)?;
        let pointer = self.as_pointer(&mut block_builder.function_builder)?;
        pointer.store(&value, block_builder)?;
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

    pub fn empty() -> Self {
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
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError> {
        let expression_handle = match &self.handle {
            GlobalVariableHandle::Handle(handle) => {
                block_builder
                    .function_builder
                    .add_expression(Expression::GlobalVariable(*handle))?
            }
            GlobalVariableHandle::Empty => ExpressionHandle::empty(),
        };
        Ok(expression_handle)
    }
}
