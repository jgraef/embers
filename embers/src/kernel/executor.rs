use std::{
    any::{
        type_name,
        TypeId,
    },
    collections::HashMap,
    sync::Arc,
};

use async_lock::Mutex;
use wgpu::{
    CommandEncoderDescriptor,
    ComputePassDescriptor,
    ComputePipeline,
};

use super::{
    Kernel,
    KernelSignature,
    Vec3,
};
use crate::{
    error::KernelError,
    gpu::Gpu,
    kernel::binding::KernelBindingBuilder,
};

#[derive(Debug)]
pub struct KernelExecutor {
    compute_pipelines: Mutex<HashMap<(TypeId, Vec3), Arc<ComputePipeline>>>,
}

impl KernelExecutor {
    pub fn new() -> Self {
        Self {
            compute_pipelines: Mutex::new(HashMap::new()),
        }
    }

    pub async fn run_kernel<'a, const D: usize, K: Kernel>(
        &self,
        gpu: &Gpu,
        args: <<K as Kernel>::Signature as KernelSignature>::Args<'a, D>,
    ) -> Result<(), KernelError> {
        let kernel_id = TypeId::of::<K>();

        let task_partition = <K as Kernel>::Signature::task_partition(&args);
        assert!(task_partition.chunk_size % K::Signature::DECLARATION.chunk_size() == 0, "chunk size must be a multiple of the chunk size required by the packing of the writable tensors");

        // fetch from cache or create compute pipeline
        // we only lock the cache for a short period to get the compute pipeline, which
        // we clone then.
        let compute_pipeline = {
            let mut compute_pipelines = self.compute_pipelines.lock().await;
            let compute_pipeline = compute_pipelines
                .entry((kernel_id, task_partition.workgroup_size))
                .or_insert_with(|| {
                    Arc::new(K::create_compute_pipeline(
                        &gpu,
                        task_partition.workgroup_size,
                    ))
                });
            compute_pipeline.clone()
        };

        // create bind group
        let mut kernel_binding_builder =
            KernelBindingBuilder::new(gpu, K::Signature::DECLARATION, task_partition.chunk_size);
        <K::Signature as KernelSignature>::build_bind_group(args, &mut kernel_binding_builder)?;
        let bind_group_layout = compute_pipeline.get_bind_group_layout(0);
        let bind_group = kernel_binding_builder.build(&bind_group_layout);

        let mut encoder = gpu
            .device()
            .create_command_encoder(&CommandEncoderDescriptor {
                label: Some(type_name::<K>()),
            });

        {
            let mut compute_pass = encoder.begin_compute_pass(&ComputePassDescriptor {
                label: Some(type_name::<K>()),
                timestamp_writes: None,
            });
            compute_pass.set_pipeline(&compute_pipeline);
            compute_pass.set_bind_group(0, &bind_group, &[]);
            compute_pass.dispatch_workgroups(
                task_partition.workgroup_count.x,
                task_partition.workgroup_count.y,
                task_partition.workgroup_count.z,
            );
        }

        gpu.queue().submit([encoder.finish()]).await;

        Ok(())
    }
}
