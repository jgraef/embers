use std::{
    borrow::Cow,
    fmt::Display,
    mem::size_of,
};

use num::integer::lcm;
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
        Encode,
        WgslType,
    },
    error::KernelError,
    tensor::buffer::create_mapped_buffer,
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
    pub fn new(gpu: &'gpu Gpu, declarations: KernelDeclaration, chunk_size: u32) -> Self {
        let mut header = Vec::with_capacity(declarations.parameters.len() + 1);
        header.push(D as i32);
        header.push(chunk_size as i32);
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

    pub fn add_parameter<P: KernelParameter>(
        &mut self,
        name: &'static str,
        value: P,
    ) -> Result<(), KernelError> {
        self.check_parameter(name, P::TYPE)?;
        value.add(&mut self.header, &mut self.body);

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
        let ty = T::Encoded::TYPE_NAME;

        let index = self.binding_index;

        let declaration = self.declarations.bindings.get(index).ok_or_else(|| {
            KernelBindingError::NoDeclaration {
                index,
                name,
                ty: ty.into(),
            }
        })?;

        if declaration.name != name {
            return Err(KernelBindingError::NameMismatch {
                index,
                name_got: name,
                name_expected: declaration.name,
            });
        }

        if declaration.encoding.ty != ty {
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

        // put num parameters into header
        let n = self.header.len();

        // create parameter buffer
        let size = ((n + self.body.len()) * size_of::<i32>())
            .try_into()
            .unwrap();
        let buffer = create_mapped_buffer(&self.gpu, size, "parameters", BufferUsages::STORAGE);
        {
            let slice = buffer.slice(..);
            let mut view = slice.get_mapped_range_mut();
            let view: &mut [i32] = bytemuck::cast_slice_mut(&mut view);
            view[0..n].copy_from_slice(&self.header);
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

impl KernelDeclaration {
    pub fn chunk_size(&self) -> u32 {
        let mut chunk_size = 1;

        for binding in self.bindings {
            if binding.read_write == KernelBindingReadWrite::ReadWrite {
                chunk_size = lcm(chunk_size, binding.encoding.num_packed);
            }
        }

        chunk_size
    }
}

#[derive(Debug, thiserror::Error)]
pub enum KernelBindingError {
    #[error("no declaration for {name}:{ty:?} at index {index}.")]
    NoDeclaration {
        index: usize,
        name: &'static str,
        ty: Cow<'static, str>,
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
        ty_got: &'static str,
        ty_expected: &'static str,
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

pub trait KernelParameter {
    const TYPE: KernelParameterType;

    fn add(&self, header: &mut Vec<i32>, body: &mut Vec<i32>);
}

macro_rules! impl_kernel_parameter {
    ($ty:ident) => {
        impl KernelParameter for $ty {
            const TYPE: KernelParameterType = KernelParameterType::Int;

            fn add(&self, header: &mut Vec<i32>, _body: &mut Vec<i32>) {
                header.push(*self as i32);
            }
        }

        impl<const N: usize> KernelParameter for [$ty; N] {
            const TYPE: KernelParameterType = KernelParameterType::Array;

            fn add(&self, header: &mut Vec<i32>, body: &mut Vec<i32>) {
                header.push(body.len() as i32);
                body.extend(self.map(|x| x as i32));
            }
        }
    };
}

impl_kernel_parameter!(i32);
impl_kernel_parameter!(u32);
impl_kernel_parameter!(isize);
impl_kernel_parameter!(usize);

#[derive(Clone, Copy, Debug)]
pub struct KernelBindingDeclaration {
    pub name: &'static str,
    pub ty: &'static str,
    pub read_write: KernelBindingReadWrite,
    pub encoding: KernelBindingEncoding,
}

impl KernelBindingDeclaration {
    pub const fn read_only<T: Encode>(name: &'static str) -> Self {
        Self {
            name,
            ty: <<T as Encode>::Primitive as WgslType>::TYPE_NAME,
            read_write: KernelBindingReadWrite::ReadOnly,
            encoding: KernelBindingEncoding::new::<T>(),
        }
    }

    pub const fn read_write<T: Encode>(name: &'static str) -> Self {
        Self {
            name,
            ty: <<T as Encode>::Primitive as WgslType>::TYPE_NAME,
            read_write: KernelBindingReadWrite::ReadWrite,
            encoding: KernelBindingEncoding::new::<T>(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
pub struct KernelBindingEncoding {
    pub ty: &'static str,
    pub num_packed: u32,
    pub decode: &'static str,
    pub encode: &'static str,
}

impl KernelBindingEncoding {
    pub const fn new<T: Encode>() -> Self {
        Self {
            ty: <<T as Encode>::Encoded as WgslType>::TYPE_NAME,
            num_packed: <T as Encode>::NUM_PACKED as u32,
            decode: <T as Encode>::WGSL_DECODE,
            encode: <T as Encode>::WGSL_ENCODE,
        }
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

    pub const fn array(name: &'static str) -> Self {
        Self {
            name,
            ty: KernelParameterType::Array,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernelParameterType {
    Int,
    Array,
}
