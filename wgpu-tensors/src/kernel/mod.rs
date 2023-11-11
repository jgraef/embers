pub mod binding;
pub mod executor;
pub mod fold;
pub mod map;

use std::{
    borrow::Cow,
    fmt::Display,
};

use askama::Template;
use wgpu::{
    ComputePipeline,
    ComputePipelineDescriptor,
    ShaderModuleDescriptor,
    ShaderSource,
};

use self::binding::{
    KernelBindingBuilder,
    KernelDeclaration,
};
use crate::{
    element::{
        Element,
        WgslType,
    },
    error::KernelError,
    gpu::Gpu,
    tensor::shape::Shape,
};

pub trait KernelSignature {
    const DECLARATION: KernelDeclaration;
    type Args<'a, const D: usize>;

    fn build_bind_group<'gpu, 'tensor, const D: usize>(
        args: Self::Args<'tensor, D>,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError>;

    fn task_partition<'a, const D: usize>(gpu: &Gpu, args: &Self::Args<'a, D>) -> TaskPartition;
}

pub trait Kernel: 'static {
    const LABEL: &'static str;
    type Template: Template;
    type Signature: KernelSignature;

    fn template(info: KernelTemplateInfo) -> Self::Template;

    fn source(work_group_size: Vec3) -> String {
        let info = KernelTemplateInfo {
            label: Self::LABEL,
            declaration: Self::Signature::DECLARATION,
            work_group_size,
        };

        let template = Self::template(info);

        template.render().expect("kernel render failed")
    }

    fn create_compute_pipeline(gpu: &Gpu, work_group_size: Vec3) -> ComputePipeline {
        let source = Self::source(work_group_size);

        tracing::debug!("shader source for {}", Self::LABEL);
        tracing::debug!("{source}");

        let module = gpu.device().create_shader_module(ShaderModuleDescriptor {
            label: Some(&format!("shader module: {}", Self::LABEL)),
            source: ShaderSource::Wgsl(source.into()),
        });

        let pipeline = gpu
            .device()
            .create_compute_pipeline(&ComputePipelineDescriptor {
                label: Some(&format!("compute pipeline: {}", Self::LABEL)),
                layout: None,
                module: &module,
                entry_point: "main",
            });

        pipeline
    }
}

#[derive(Debug)]
pub struct KernelTemplateInfo {
    pub label: &'static str,
    pub work_group_size: Vec3,
    pub declaration: KernelDeclaration,
}

#[derive(Debug)]
pub struct BindingTemplate<'a> {
    pub binding_id: BindingId,
    pub rw: BindingReadWrite,
    pub name: Cow<'a, str>,
    pub data_type: WgslType,
}

impl<'a> BindingTemplate<'a> {
    pub fn new<T: Element>(
        binding_id: BindingId,
        rw: BindingReadWrite,
        name: impl Into<Cow<'a, str>>,
    ) -> Self {
        Self {
            binding_id,
            rw,
            name: name.into(),
            data_type: T::WGSL_TYPE,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BindingReadWrite {
    ReadOnly,
    ReadWrite,
}

impl Display for BindingReadWrite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BindingReadWrite::ReadOnly => "read",
            BindingReadWrite::ReadWrite => "read_write",
        };
        write!(f, "{s}")
    }
}

type BindingId = u32;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Vec3 {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}

impl Vec3 {
    pub fn product(&self) -> u32 {
        self.x * self.y * self.z
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TaskPartition {
    pub workgroup_size: Vec3,
    pub workgroup_count: Vec3,
    pub chunk_size: u32,
}

impl TaskPartition {
    pub fn from_shape(gpu: &Gpu, shape: impl Shape) -> Self {
        let limits = gpu.limits();

        let output_size = shape.size().try_into().unwrap();
        let max_workgroup_size = limits.max_compute_workgroup_size_x;
        //let max_workgroup_count = limits.max_compute_invocations_per_workgroup;

        if output_size <= max_workgroup_size {
            TaskPartition {
                workgroup_size: Vec3 {
                    x: output_size,
                    y: 1,
                    z: 1,
                },
                workgroup_count: Vec3 { x: 1, y: 1, z: 1 },
                chunk_size: 1,
            }
        }
        else {
            todo!();
        }
    }
}
