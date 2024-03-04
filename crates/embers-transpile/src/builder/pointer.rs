use std::marker::{
    ConstParamTy,
    PhantomData,
};

use naga::StorageAccess;

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

pub struct Pointer<T: ShaderType, const A: AddressSpace> {
    _ty: PhantomData<T>,
}

impl<T: ShaderType, const A: AddressSpace> ShaderType for Pointer<T, A> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        let ty = module_builder.get_type_by_id_or_add_it::<T>()?;
        let base = match ty {
            TypeHandle::Empty => return Ok(TypeHandle::Empty),
            TypeHandle::Func(_) => return Err(BuilderError::NotANagaType { ty }),
            TypeHandle::Type(ty) => ty,
        };
        let space = A.into();
        Ok(module_builder.add_type::<Self>(None, naga::TypeInner::Pointer { base, space }))
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
