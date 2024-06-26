use naga::{
    Function,
    Handle,
    Type,
};

use super::{
    error::BuilderError,
    module::ModuleBuilder,
};

pub trait ShaderType: 'static {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Result<TypeHandle, BuilderError>;
}

pub trait Width {
    const WIDTH: u32;
}

pub trait AlignTo {
    const ALIGN_TO: u32;
}

pub trait Name {
    const NAME: &'static str;
}

pub trait ScalarKind {
    const KIND: naga::ScalarKind;
}

pub(crate) fn scalar_to_naga<T: ScalarKind + Width>() -> naga::Scalar {
    naga::Scalar {
        kind: <T as ScalarKind>::KIND,
        width: <T as Width>::WIDTH as u8,
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub struct TypeHandle {
    pub(crate) data: Option<Handle<Type>>,
    pub(crate) code: Option<Handle<Function>>,
}

impl From<Handle<Type>> for TypeHandle {
    fn from(value: Handle<Type>) -> Self {
        Self {
            data: Some(value),
            code: None,
        }
    }
}

impl From<Handle<Function>> for TypeHandle {
    fn from(value: Handle<Function>) -> Self {
        Self {
            data: None,
            code: Some(value),
        }
    }
}

impl TypeHandle {
    pub fn get_data(&self) -> Option<Handle<Type>> {
        self.data.as_ref().copied()
    }

    pub fn try_get_data(&self) -> Result<Handle<Type>, BuilderError> {
        self.get_data()
            .ok_or_else(|| BuilderError::NotANagaType { ty: *self })
    }

    pub fn get_code(&self) -> Option<Handle<Function>> {
        self.code.as_ref().copied()
    }

    pub fn try_get_code(&self) -> Result<Handle<Function>, BuilderError> {
        self.get_code()
            .ok_or_else(|| BuilderError::NotAFunction { ty: *self })
    }

    pub fn is_zero_sized(&self) -> bool {
        self.data.is_none()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Mutability {
    Immutable,
    Mutable,
}
