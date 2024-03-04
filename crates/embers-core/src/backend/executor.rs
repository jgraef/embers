use std::{
    any::TypeId,
    collections::HashMap,
    sync::Arc,
};

use async_lock::Mutex;
use wgpu::ComputePipeline;

use crate::utils::Vec3;

#[derive(Debug)]
pub struct Executor {
    compute_pipelines: Mutex<HashMap<(TypeId, Vec3), Arc<ComputePipeline>>>,
}

impl Executor {
    pub fn new() -> Self {
        Self {
            compute_pipelines: Mutex::new(HashMap::new()),
        }
    }
}
