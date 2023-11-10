pub mod binary;
pub mod executor;
pub mod trinary;
pub mod unary;

use std::{
    borrow::Cow,
    fmt::Display,
};

use askama::Template;
use wgpu::{
    util::{
        BufferInitDescriptor,
        DeviceExt,
    },
    BindGroup,
    BindGroupDescriptor,
    BindGroupEntry,
    BindGroupLayout,
    BufferUsages,
    ComputePipeline,
    ComputePipelineDescriptor,
    ShaderModuleDescriptor,
    ShaderSource,
};

use crate::{
    element::Element,
    error::{
        GpuMismatch,
        KernelError,
    },
    gpu::Gpu,
    tensor::{
        shape::Shape,
        strider::contiguous_strides,
        Tensor,
    },
};

pub trait KernelArgs<'a, const D: usize> {
    fn create_bind_group(
        &'a self,
        bind_group_builder: &mut BindGroupBuilder<'a, '_, D>,
    ) -> Result<(), KernelError>;

    fn shape(&self) -> [usize; D];
}

pub trait KernelSignature {
    type Args<'a, const D: usize>: KernelArgs<'a, D>;

    fn binding_template() -> Vec<BindingTemplate<'static>>;

    fn info_binding() -> BindingId;
}

pub trait Kernel<S: KernelSignature>: 'static {
    const LABEL: &'static str;
    const BODY: &'static str;

    fn source(work_group_size: Vec3) -> String {
        KernelTemplate {
            label: Self::LABEL,
            bindings: S::binding_template(),
            info_binding_id: S::info_binding(),
            work_group_size,
            body: Self::BODY,
        }
        .render()
        .expect("kernel render failed")
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

#[derive(Debug, Template)]
#[template(path = "kernel.wgsl")]
pub struct KernelTemplate<'a> {
    pub label: &'a str,
    pub bindings: Vec<BindingTemplate<'a>>,
    pub info_binding_id: BindingId,
    pub work_group_size: Vec3,
    pub body: &'a str,
}

#[derive(Debug)]
pub struct BindingTemplate<'a> {
    pub binding_id: BindingId,
    pub rw: BindingReadWrite,
    pub name: Cow<'a, str>,
    pub data_type: &'a str,
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

    pub fn read_only<T: Element>(binding_id: BindingId, name: &'a str) -> Self {
        Self::new::<T>(binding_id, BindingReadWrite::ReadOnly, name)
    }

    pub fn read_write<T: Element>(binding_id: BindingId, name: &'a str) -> Self {
        Self::new::<T>(binding_id, BindingReadWrite::ReadWrite, name)
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

pub struct BindGroupBuilder<'tensor, 'gpu, const D: usize> {
    gpu: &'gpu Gpu,
    shape_data: Vec<i32>,
    bind_group_entries: Vec<BindGroupEntry<'tensor>>,
    info_binding_id: Option<BindingId>,
}

impl<'tensor, 'gpu, const D: usize> BindGroupBuilder<'tensor, 'gpu, D> {
    pub fn new(gpu: &'gpu Gpu, chunk_size: u32, operation_shape: [usize; D]) -> Self {
        let mut shape_data = vec![D as i32, chunk_size as i32];
        shape_data.push(0); // offset, unused
        shape_data.extend(operation_shape.map(|x| x as i32));
        shape_data.extend(contiguous_strides(&operation_shape).map(|x| x as i32));

        Self {
            gpu,
            shape_data,
            bind_group_entries: vec![],
            info_binding_id: None,
        }
    }

    pub fn add_tensor_binding<T: Element>(
        &mut self,
        binding_id: BindingId,
        tensor: &'tensor Tensor<D, T>,
    ) -> Result<&mut Self, GpuMismatch> {
        self.gpu.check_same(&tensor.gpu)?;

        self.shape_data.push(tensor.strider.offset() as _);
        self.shape_data
            .extend(tensor.strider.shape().map(|x| x as i32));
        self.shape_data
            .extend(tensor.strider.strides().map(|x| x as i32));

        self.bind_group_entries.push(BindGroupEntry {
            binding: binding_id,
            resource: tensor.buffer.as_binding(),
        });

        Ok(self)
    }

    pub fn add_info_binding(&mut self, binding_id: BindingId) -> &mut Self {
        self.info_binding_id = Some(binding_id);
        self
    }

    pub fn build(self, bind_group_layout: &BindGroupLayout) -> BindGroup {
        let info_buffer = self.gpu.device().create_buffer_init(&BufferInitDescriptor {
            label: Some("info buffer"),
            contents: bytemuck::cast_slice(&self.shape_data),
            usage: BufferUsages::STORAGE | BufferUsages::COPY_SRC,
        });

        let mut bind_group_entries = self.bind_group_entries;

        bind_group_entries.push(BindGroupEntry {
            binding: self.info_binding_id.expect("no info binding"),
            resource: info_buffer.as_entire_binding(),
        });

        let bind_group = self.gpu.device().create_bind_group(&BindGroupDescriptor {
            label: None,
            layout: bind_group_layout,
            entries: &bind_group_entries,
        });

        bind_group
    }
}

type BindingId = u32;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Vec3 {
    pub x: u32,
    pub y: u32,
    pub z: u32,
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
