use crate::{
    gpu::Gpu,
    kernel::binding::{
        KernelBindingError,
        KernelParameterError,
    },
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("no adapter found")]
    NoAdapter,

    #[error("could not request device from adapter")]
    RequestDeviceError(#[from] wgpu::RequestDeviceError),

    #[error("size mismatch: expected {expected} elements, got {got}.")]
    SizeMismatch { expected: usize, got: usize },

    #[error("failed to map tensor to host memory")]
    TensorMap(#[from] crate::tensor::buffer::MapTensorBufferError),

    #[error("gpu mismatch")]
    GpuMismatch(#[from] GpuMismatch),
}

#[derive(Debug, thiserror::Error)]
#[error("gpu mismatch: '{}' != '{}'", first.name(), second.name())]
pub struct GpuMismatch {
    pub first: Gpu,
    pub second: Gpu,
}

#[derive(Debug, thiserror::Error)]
#[error("shape mismatch: {first:?} != {second:?}")]
pub struct ShapeMismatch {
    pub first: Vec<usize>,
    pub second: Vec<usize>,
}

impl ShapeMismatch {
    pub fn new(first: &[usize], second: &[usize]) -> Self {
        Self {
            first: first.to_vec(),
            second: second.to_vec(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum KernelError {
    #[error("shape mismatch")]
    ShapeMismatch(#[from] ShapeMismatch),

    #[error("gpu mismatch")]
    GpuMismatch(#[from] GpuMismatch),

    #[error("kernel parameter error")]
    KernelParameter(#[from] KernelParameterError),

    #[error("kernel binding error")]
    KernelBinding(#[from] KernelBindingError),

    #[error("invalid axis")]
    InvalidAxis(#[from] InvalidAxis),
}

#[derive(Debug, thiserror::Error)]
#[error("expected {expected} dimensions, but got {got}")]
pub struct DimensionMismatch {
    pub expected: usize,
    pub got: usize,
}

#[derive(Debug, thiserror::Error)]
#[error("invalid axis {axis} for {dimensions}-dimensional tensor")]
pub struct InvalidAxis {
    pub axis: usize,
    pub dimensions: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum PermuteError {
    #[error("invalid permutation: {permutation:?}")]
    InvalidAxis {
        permutation: Vec<usize>,
        #[source]
        invalid_axis: InvalidAxis,
    },

    #[error("invalid permutation: {permutation:?}. all axis must be specified exactly once.")]
    InvalidPermutation { permutation: Vec<usize> },
}

#[derive(Debug, thiserror::Error)]
#[error("can't expand axis {axis} from old shape {shape:?} to new shape {expanded_shape:?}")]
pub struct BroadcastError {
    pub axis: usize,
    pub shape: Vec<usize>,
    pub expanded_shape: Vec<usize>,
}

#[derive(Debug, thiserror::Error)]
#[error("slice results in empty tensor. axis {axis} starts at {start} and ends at {end}.")]
pub struct SliceError {
    pub axis: usize,
    pub start: usize,
    pub end: usize,
}
