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