use std::{
    fmt::Display,
    mem::size_of,
};

use wgpu::{
    BindGroup,
    BindGroupDescriptor,
    BindGroupEntry,
    BindGroupLayout,
    BufferUsages,
};

use super::BindingId;
use crate::{
    element::{
        Element,
        WgslType,
    },
    error::KernelError,
    tensor::{
        buffer::create_mapped_buffer,
        strider::Strider,
    },
    Gpu,
    Tensor,
};

pub struct KernelBindingBuilder<'gpu, 'tensor, const D: usize> {
    gpu: &'gpu Gpu,

    declarations: KernelDeclaration,
    parameter_index: usize,
    header: Vec<i32>,
    body: Vec<i32>,

    bind_group_entries: Vec<BindGroupEntry<'tensor>>,
    binding_index: usize,
    next_binding_id: BindingId,
}

impl<'gpu, 'tensor, const D: usize> KernelBindingBuilder<'gpu, 'tensor, D> {
    pub fn new(gpu: &'gpu Gpu, declarations: KernelDeclaration) -> Self {
        let mut header = Vec::with_capacity(declarations.parameters.len() + 1);
        header.push(D as i32);
        header.push(0);
        let body = Vec::with_capacity(declarations.parameters.len() * D);

        let bind_group_entries = Vec::with_capacity(declarations.bindings.len() + 1);

        Self {
            gpu,
            declarations,
            parameter_index: 0,
            header,
            body,
            bind_group_entries,
            binding_index: 0,
            next_binding_id: 1,
        }
    }

    pub fn add_parameter<'a>(
        &mut self,
        name: &'static str,
        value: impl Into<KernelParameter<D>>,
    ) -> Result<(), KernelError> {
        match value.into() {
            KernelParameter::Int(value) => {
                self.check_parameter(name, KernelParameterType::Int)?;
                self.header.push(value);
            }
            KernelParameter::Shaped(value) => {
                self.check_parameter(name, KernelParameterType::Shaped)?;
                self.header.push(self.body.len() as i32);
                self.body.extend(value.map(|x| x as i32));
            }
            KernelParameter::Strider(strider) => {
                self.check_parameter(name, KernelParameterType::Strider)?;
                self.header.push(self.body.len() as i32);
                self.body.push(strider.offset() as i32);
                self.body.extend(strider.shape().map(|x| x as i32));
                self.body.extend(strider.strides().map(|x| x as i32));
            }
        }

        self.parameter_index += 1;

        Ok(())
    }

    fn check_parameter(
        &self,
        name: &'static str,
        ty: KernelParameterType,
    ) -> Result<(), KernelParameterError> {
        let index = self.parameter_index;

        let declaration = self
            .declarations
            .parameters
            .get(index)
            .ok_or_else(|| KernelParameterError::NoDeclaration { index, name, ty })?;

        if declaration.name != name {
            return Err(KernelParameterError::NameMismatch {
                index,
                name_got: name,
                name_expected: declaration.name,
            });
        }

        if declaration.ty != ty {
            return Err(KernelParameterError::TypeMismatch {
                index,
                name,
                ty_got: ty,
                ty_expected: declaration.ty,
            });
        }

        Ok(())
    }

    pub fn add_binding<T: Element>(
        &mut self,
        name: &'static str,
        tensor: &'tensor Tensor<D, T>,
    ) -> Result<(), KernelError> {
        self.gpu.check_same(&tensor.gpu)?;
        self.check_binding::<T>(name)?;

        let binding_id = self.next_binding_id;

        self.bind_group_entries.push(BindGroupEntry {
            binding: binding_id,
            resource: tensor.buffer.as_binding(),
        });

        self.next_binding_id += 1;
        self.binding_index += 1;

        Ok(())
    }

    fn check_binding<T: Element>(&self, name: &'static str) -> Result<(), KernelBindingError> {
        let ty = T::WGSL_TYPE;

        let index = self.binding_index;

        let declaration = self
            .declarations
            .bindings
            .get(index)
            .ok_or_else(|| KernelBindingError::NoDeclaration { index, name, ty })?;

        if declaration.name != name {
            return Err(KernelBindingError::NameMismatch {
                index,
                name_got: name,
                name_expected: declaration.name,
            });
        }

        if declaration.ty != ty {
            return Err(KernelBindingError::TypeMismatch {
                index,
                name,
                ty_got: ty,
                ty_expected: declaration.ty,
            });
        }

        Ok(())
    }

    pub fn build(self, bind_group_layout: &BindGroupLayout) -> BindGroup {
        let mut bind_group_entries = self.bind_group_entries;
        let mut header = self.header;

        // put num parameters into header
        let n = header.len();
        header[1] = (n - 2).try_into().unwrap();

        // create parameter buffer
        let size = ((header.len() + self.body.len()) * size_of::<i32>())
            .try_into()
            .unwrap();
        let buffer = create_mapped_buffer(&self.gpu, size, "parameters", BufferUsages::STORAGE);
        {
            let slice = buffer.slice(..);
            let mut view = slice.get_mapped_range_mut();
            let view: &mut [i32] = bytemuck::cast_slice_mut(&mut view);
            view[0..n].copy_from_slice(&header);
            view[n..n + self.body.len()].copy_from_slice(&self.body);
            tracing::debug!("kernel parameters: {:?}", &view[..]);
        }
        buffer.unmap();

        // add parameter buffer as first binding
        bind_group_entries.push(BindGroupEntry {
            binding: 0,
            resource: buffer.as_entire_binding(),
        });

        // create bind group
        let bind_group = self.gpu.device().create_bind_group(&BindGroupDescriptor {
            label: None,
            layout: bind_group_layout,
            entries: &bind_group_entries,
        });

        bind_group
    }
}

#[derive(Clone, Copy, Debug)]
pub struct KernelDeclaration {
    pub bindings: &'static [KernelBindingDeclaration],
    pub parameters: &'static [KernelParameterDeclaration],
}

#[derive(Debug, thiserror::Error)]
pub enum KernelBindingError {
    #[error("no declaration for {name}:{ty:?} at index {index}.")]
    NoDeclaration {
        index: usize,
        name: &'static str,
        ty: WgslType,
    },

    #[error("expected field {name_expected}, but got {name_got}.")]
    NameMismatch {
        index: usize,
        name_got: &'static str,
        name_expected: &'static str,
    },

    #[error("expected type {ty_expected:?} but got {ty_got:?} for {name} at index {index}.")]
    TypeMismatch {
        index: usize,
        name: &'static str,
        ty_got: WgslType,
        ty_expected: WgslType,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum KernelParameterError {
    #[error("no declaration for {name}:{ty:?} at index {index}.")]
    NoDeclaration {
        index: usize,
        name: &'static str,
        ty: KernelParameterType,
    },

    #[error("expected field {name_expected}, but got {name_got}.")]
    NameMismatch {
        index: usize,
        name_got: &'static str,
        name_expected: &'static str,
    },

    #[error("expected type {ty_expected:?} but got {ty_got:?} for {name} at index {index}.")]
    TypeMismatch {
        index: usize,
        name: &'static str,
        ty_got: KernelParameterType,
        ty_expected: KernelParameterType,
    },
}

#[derive(Clone, Debug)]
pub enum KernelParameter<const D: usize> {
    Int(i32),
    Shaped([usize; D]),
    Strider(Strider<D>),
}

impl<const D: usize> From<i32> for KernelParameter<D> {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl<const D: usize> From<usize> for KernelParameter<D> {
    fn from(value: usize) -> Self {
        Self::Int(value.try_into().unwrap())
    }
}

impl<const D: usize> From<[usize; D]> for KernelParameter<D> {
    fn from(value: [usize; D]) -> Self {
        Self::Shaped(value)
    }
}

impl<'a, const D: usize> From<[isize; D]> for KernelParameter<D> {
    fn from(value: [isize; D]) -> Self {
        Self::Shaped(value.map(|x| x.try_into().unwrap()))
    }
}

impl<const D: usize> From<Strider<D>> for KernelParameter<D> {
    fn from(value: Strider<D>) -> Self {
        Self::Strider(value)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct KernelBindingDeclaration {
    pub name: &'static str,
    pub ty: WgslType,
    pub read_write: KernelBindingReadWrite,
}

impl KernelBindingDeclaration {
    pub const fn read_only<T: Element>(name: &'static str) -> Self {
        Self {
            name,
            ty: T::WGSL_TYPE,
            read_write: KernelBindingReadWrite::ReadOnly,
        }
    }

    pub const fn read_write<T: Element>(name: &'static str) -> Self {
        Self {
            name,
            ty: T::WGSL_TYPE,
            read_write: KernelBindingReadWrite::ReadWrite,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum KernelBindingReadWrite {
    ReadOnly,
    ReadWrite,
}

impl Display for KernelBindingReadWrite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::ReadOnly => "read",
            Self::ReadWrite => "read_write",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct KernelParameterDeclaration {
    pub name: &'static str,
    pub ty: KernelParameterType,
}

impl KernelParameterDeclaration {
    pub const fn int(name: &'static str) -> Self {
        Self {
            name,
            ty: KernelParameterType::Int,
        }
    }

    pub const fn shaped(name: &'static str) -> Self {
        Self {
            name,
            ty: KernelParameterType::Shaped,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernelParameterType {
    Int,
    Shaped,
    Strider,
}
