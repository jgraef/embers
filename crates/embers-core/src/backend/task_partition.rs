use crate::{
    element::{
        block::Block,
        Element,
    },
    utils::Vec3,
    Tensor,
};

#[derive(Copy, Clone, Debug)]
pub struct TaskPartition {
    pub workgroup_size: Vec3,
    pub workgroup_count: Vec3,
    pub chunk_size: u32,
}

impl TaskPartition {
    pub fn for_result<const D: usize, T: Element>(tensor: &Tensor<D, T>) -> Self {
        let limits = tensor.gpu.limits();

        let output_size = T::Block::encoded_size(tensor.size());
        let chunk_size = T::Block::NUM_PACKED;

        let workgroup_size = output_size.div_ceil(chunk_size) as u32;

        if workgroup_size <= limits.max_compute_workgroup_size_x {
            TaskPartition {
                workgroup_size: Vec3 {
                    x: workgroup_size,
                    y: 1,
                    z: 1,
                },
                workgroup_count: Vec3 { x: 1, y: 1, z: 1 },
                chunk_size: chunk_size as u32,
            }
        }
        else {
            todo!();
        }
    }
}
