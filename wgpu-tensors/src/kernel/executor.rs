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
    TaskPartition,
    Vec3,
};
use crate::{
    error::KernelError,
    gpu::Gpu,
    kernel::{
        BindGroupBuilder,
        KernelArgs,
    },
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

    pub async fn run_kernel<'a, const D: usize, K: Kernel<S>, S: KernelSignature>(
        &self,
        gpu: &Gpu,
        args: &'a <S as KernelSignature>::Args<'a, D>,
    ) -> Result<(), KernelError> {
        let kernel_id = TypeId::of::<K>();
        let operation_shape = args.shape();
        let task_partition = TaskPartition::from_shape(gpu, &operation_shape);
        //let task_partition = K::task_partition(gpu, args);
        assert_eq!(task_partition.chunk_size, 1, "todo");

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
        let mut bind_group_builder =
            BindGroupBuilder::new(gpu, task_partition.chunk_size, operation_shape);
        args.create_bind_group(&mut bind_group_builder)?;
        let bind_group_layout = compute_pipeline.get_bind_group_layout(0);
        let bind_group = bind_group_builder.build(&bind_group_layout);

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
