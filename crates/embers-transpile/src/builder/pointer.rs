use std::marker::PhantomData;

use naga::{
    Expression,
    Statement,
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
        Ok(module_builder.add_type::<Self>(None, naga::TypeInner::Pointer { base, space }))
    }
}

impl<T: ShaderType, A: AddressSpace> ExpressionHandle<Pointer<T, A>> {
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

impl<T: ShaderType, A: AddressSpace> Dereference for ExpressionHandle<Pointer<T, A>> {
    type Target = ExpressionHandle<T>;

    fn dereference(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Target, BuilderError> {
        self.load(function_builder)
    }
}
