use std::sync::Arc;

use wgpu::{
    util::initialize_adapter_from_env_or_default,
    Adapter,
    DeviceDescriptor,
    Instance,
    Limits,
};
use wgpu_async::{
    AsyncDevice,
    AsyncQueue,
};

use super::executor::Executor;
use crate::error::{
    Error,
    GpuMismatch,
};

#[derive(Debug)]
struct Inner {
    adapter: Adapter,
    device: AsyncDevice,
    queue: AsyncQueue,
    executor: Executor,
    limits: Limits,
}

#[derive(Debug, Clone)]
pub struct Gpu {
    inner: Arc<Inner>,
}

impl Gpu {
    pub async fn new() -> Result<Self, Error> {
        let instance = Instance::default();
        let adapter = initialize_adapter_from_env_or_default(&instance, None)
            .await
            .ok_or(Error::NoAdapter)?;
        Self::from_adapter(adapter).await
    }

    pub async fn from_adapter(adapter: Adapter) -> Result<Self, Error> {
        let (device, queue) = adapter
            .request_device(
                &DeviceDescriptor {
                    label: None,
                    required_features: Default::default(),
                    required_limits: Default::default(),
                },
                None,
            )
            .await?;

        let (device, queue) = wgpu_async::wrap(Arc::new(device), Arc::new(queue));

        let executor = Executor::new();

        Ok(Self {
            inner: Arc::new(Inner {
                adapter,
                device,
                queue,
                executor,
                limits: Limits::default(),
            }),
        })
    }

    pub(crate) fn device(&self) -> &AsyncDevice {
        &self.inner.device
    }

    pub(crate) fn queue(&self) -> &AsyncQueue {
        &self.inner.queue
    }

    pub(crate) fn executor(&self) -> &Executor {
        &self.inner.executor
    }

    pub(crate) fn limits(&self) -> &Limits {
        &self.inner.limits
    }

    pub fn is_same(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }

    pub(crate) fn check_same(&self, other: &Self) -> Result<(), GpuMismatch> {
        if self.is_same(other) {
            Ok(())
        }
        else {
            Err(GpuMismatch {
                first: self.clone(),
                second: other.clone(),
            })
        }
    }

    pub fn name(&self) -> String {
        self.inner.adapter.get_info().name
    }
}
