use std::marker::{
    ConstParamTy,
    PhantomData,
};

use naga::StorageAccess;

use crate::{
    builder::{
        ModuleBuilder,
        TypeHandle,
    },
    RicslType,
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

pub struct Pointer<T: RicslType, const A: AddressSpace> {
    _ty: PhantomData<T>,
}

impl<T: RicslType, const A: AddressSpace> RicslType for Pointer<T, A> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> TypeHandle {
        let base = module_builder.get_type_by_id_or_add_it::<T>();
        let base = base
            .get_type()
            .expect("fixme: pointer base is not a naga type");
        let space = A.into();
        module_builder.add_intrinsic_type::<Self>(None, naga::TypeInner::Pointer { base, space })
    }
}
