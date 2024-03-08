use std::marker::PhantomData;

use naga::{
    Expression,
    Statement,
};

use super::{
    block::BlockBuilder,
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
    function::FunctionBuilder,
    module::ModuleBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
    },
};

pub trait AddressSpace: 'static {
    fn to_naga() -> naga::AddressSpace;
}

pub trait InAddressSpace {
    type AddressSpace: AddressSpace;
}

pub mod address_space {
    use naga::StorageAccess;

    use super::AddressSpace;

    macro_rules! impl_address_space {
        ($name:ident) => {
            pub struct $name;

            impl AddressSpace for $name {
                fn to_naga() -> naga::AddressSpace {
                    naga::AddressSpace::$name
                }
            }
        };
    }

    impl_address_space!(Function);
    impl_address_space!(Private);
    impl_address_space!(WorkGroup);
    impl_address_space!(Uniform);
    impl_address_space!(Handle);
    impl_address_space!(PushConstant);

    pub struct StorageRead;
    impl AddressSpace for StorageRead {
        fn to_naga() -> naga::AddressSpace {
            naga::AddressSpace::Storage {
                access: StorageAccess::LOAD,
            }
        }
    }

    pub struct StorageReadWrite;
    impl AddressSpace for StorageReadWrite {
        fn to_naga() -> naga::AddressSpace {
            naga::AddressSpace::Storage {
                access: StorageAccess::LOAD | StorageAccess::STORE,
            }
        }
    }
}

pub struct Pointer<T: ShaderType + ?Sized, A: AddressSpace> {
    // todo: put ExpressionHandle in here and implement AsExpression and FromExpression
    handle: ExpressionHandle<Self>,
    _ty: PhantomData<T>,
    _space: PhantomData<A>,
}

impl<T: ShaderType, A: AddressSpace> ShaderType for Pointer<T, A> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError> {
        let base = module_builder.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data()
        else {
            return Ok(module_builder.add_empty_type::<Self>());
        };
        let space = A::to_naga();
        Ok(module_builder.add_naga_type::<Self>(None, naga::TypeInner::Pointer { base, space }))
    }
}

impl<T: ShaderType, A: AddressSpace> AsExpression<Pointer<T, A>> for Pointer<T, A> {
    fn as_expression(
        &self,
        _block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError> {
        Ok(self.handle.clone())
    }
}

/*
impl<T: ShaderType, A: AddressSpace> FromExpression<Pointer<T, A>> for Pointer<T, A> {
    fn from_expression(handle: ExpressionHandle<Pointer<T, A>>) -> Result<Self, BuilderError> {
        Ok(Self {
            handle,
            _ty: PhantomData,
            _space: PhantomData,
        })
    }
}

impl<T: ShaderType, A: AddressSpace> IntoExpression<Pointer<T, A>> for Pointer<T, A> {
    fn into_expression(self) -> ExpressionHandle<Self> {
        self.handle
    }
}
 */

impl<T: ShaderType, A: AddressSpace> ExpressionHandle<Pointer<T, A>> {
    pub fn load(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let expr = match self {
            ExpressionHandle::Handle { handle, .. } => {
                let expr = block_builder
                    .function_builder
                    .add_expression(Expression::Load { pointer: *handle })?;
                block_builder.add_emit(&expr)?;
                expr
            }
            ExpressionHandle::Empty { _ty } => ExpressionHandle::from_empty(),
        };

        Ok(expr)
    }

    pub fn store(
        &self,
        value: &ExpressionHandle<T>,
        block_builder: &mut BlockBuilder,
    ) -> Result<(), BuilderError> {
        if let Some(handle) = self.get_handle() {
            block_builder.add_emit(value)?;
            let value = value.try_get_handle()?;
            block_builder.add_statement(Statement::Store {
                pointer: handle,
                value,
            })?;
        }
        else {
            // we store a empty type (e.g. ()) by doing nothing
        }

        Ok(())
    }
}

pub trait Dereference {
    type Target;

    fn dereference(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Self::Target>, BuilderError>;
}

pub trait AsPointer {
    type Base: ShaderType;
    type AddressSpace: AddressSpace;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<Self::Base, Self::AddressSpace>>, BuilderError>;
}

impl<T: ShaderType, A: AddressSpace> AsPointer for ExpressionHandle<Pointer<T, A>> {
    type Base = T;
    type AddressSpace = A;

    fn as_pointer(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<Self::Base, Self::AddressSpace>>, BuilderError> {
        Ok(*self)
    }
}

impl<T: ShaderType, A: AddressSpace> Dereference for ExpressionHandle<Pointer<T, A>> {
    type Target = T;

    fn dereference(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Self::Target>, BuilderError> {
        self.load(block_builder)
    }
}

pub struct DeferredDereference<T: ShaderType, A: AddressSpace> {
    pointer: ExpressionHandle<Pointer<T, A>>,
}

impl<T: ShaderType, A: AddressSpace> DeferredDereference<T, A> {
    pub fn new(pointer: ExpressionHandle<Pointer<T, A>>) -> Self {
        Self { pointer }
    }
}

impl<T: ShaderType, A: AddressSpace> AsPointer for DeferredDereference<T, A> {
    type Base = T;
    type AddressSpace = A;

    fn as_pointer(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError> {
        Ok(self.pointer)
    }
}

// todo: do a load here and return ExpressionHandle<T>
impl<T: ShaderType, A: AddressSpace> AsExpression<Pointer<T, A>> for DeferredDereference<T, A> {
    fn as_expression(
        &self,
        _block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError> {
        Ok(self.pointer)
    }
}
