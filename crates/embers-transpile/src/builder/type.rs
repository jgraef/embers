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
    const WIDTH: usize;
}

#[derive(Copy, Clone, Debug)]
pub enum TypeHandle {
    Empty,
    Type(Handle<Type>),
    Func(Handle<Function>),
}

impl From<Handle<Type>> for TypeHandle {
    fn from(value: Handle<Type>) -> Self {
        Self::Type(value)
    }
}

impl From<Handle<Function>> for TypeHandle {
    fn from(value: Handle<Function>) -> Self {
        Self::Func(value)
    }
}

impl TypeHandle {
    pub fn get_type(&self) -> Option<Handle<Type>> {
        match self {
            Self::Type(h) => Some(*h),
            _ => None,
        }
    }

    pub fn try_get_type(&self) -> Result<Handle<Type>, BuilderError> {
        self.get_type()
            .ok_or_else(|| BuilderError::NotANagaType { ty: *self })
    }

    pub fn get_func(&self) -> Option<Handle<Function>> {
        match self {
            Self::Func(f) => Some(*f),
            _ => None,
        }
    }

    pub fn try_get_func(&self) -> Result<Handle<Function>, BuilderError> {
        self.get_func()
            .ok_or_else(|| BuilderError::NotAFunction { ty: *self })
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}
