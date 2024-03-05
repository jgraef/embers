use std::marker::{
    ConstParamTy,
    PhantomData,
};

use naga::{
    Expression,
    Statement,
    StorageAccess,
};

use super::{
    error::BuilderError,
    expression::ExpressionHandle,
    function::FunctionBuilder,
    module::ModuleBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, ConstParamTy)]
pub enum AddressSpace {
    Function,
    Private,
    WorkGroup,
    Uniform,
    Storage { load: bool, store: bool },
    Handle,
    PushConstant,
}

impl From<AddressSpace> for naga::AddressSpace {
    fn from(value: AddressSpace) -> Self {
        match value {
            AddressSpace::Function => naga::AddressSpace::Function,
            AddressSpace::Private => naga::AddressSpace::Private,
            AddressSpace::WorkGroup => naga::AddressSpace::WorkGroup,
            AddressSpace::Uniform => naga::AddressSpace::Uniform,
            AddressSpace::Storage { load, store } => {
                let mut access = StorageAccess::empty();
                if load {
                    access |= StorageAccess::LOAD
                }
                if store {
                    access |= StorageAccess::STORE
                }
                naga::AddressSpace::Storage { access }
            }
            AddressSpace::Handle => naga::AddressSpace::Handle,
            AddressSpace::PushConstant => naga::AddressSpace::PushConstant,
        }
    }
}

pub struct Pointer<T: ShaderType + ?Sized, const A: AddressSpace> {
    _ty: PhantomData<T>,
}

impl<T: ShaderType, const A: AddressSpace> ShaderType for Pointer<T, A> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        let base = module_builder.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data() else { return Ok(module_builder.add_empty_type::<Self>()); };
        let space = A.into();
        Ok(module_builder.add_type::<Self>(None, naga::TypeInner::Pointer { base, space }))
    }
}

impl<T: ShaderType, const A: AddressSpace> ExpressionHandle<Pointer<T, A>> {
    pub fn load(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let expr = match self {
            ExpressionHandle::Handle { handle, .. } => {
                let expr = function_builder.add_expression(Expression::Load { pointer: *handle });
                function_builder.add_emit(&expr)?;
                expr
            }
            ExpressionHandle::Empty { _ty } => ExpressionHandle::from_empty(),
        };

        Ok(expr)
    }

    pub fn store(
        &self,
        value: &ExpressionHandle<T>,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError> {
        match self {
            ExpressionHandle::Handle { handle, _ty } => {
                let value = value
                    .get_handle()
                    .expect("expected value to have a naga handle");
                function_builder.add_statement(Statement::Store {
                    pointer: *handle,
                    value,
                });
            }
            ExpressionHandle::Empty { _ty } => {
                // we store a empty type (e.g. ()) by doing nothing
            }
        }

        Ok(())
    }
}

pub trait Dereference {
    type Target;

    fn dereference(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Target, BuilderError>;
}

pub trait AsPointer {
    type Pointer;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError>;
}

pub trait HasAddressSpace {
    const ADDRESS_SPACE: AddressSpace;
}

impl<T: ShaderType, const A: AddressSpace> Dereference for ExpressionHandle<Pointer<T, A>> {
    type Target = ExpressionHandle<T>;

    fn dereference(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Target, BuilderError> {
        self.load(function_builder)
    }
}
