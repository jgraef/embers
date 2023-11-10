use std::{
    ops::{
        Deref,
        DerefMut,
    },
    sync::{
        atomic::{
            AtomicUsize,
            Ordering,
        },
        Arc,
    },
};

use ouroboros::self_referencing;
use wgpu::{
    BindingResource,
    BufferAddress,
    BufferAsyncError,
    BufferDescriptor,
    BufferUsages,
    BufferView,
    BufferViewMut,
    MapMode,
};
use wgpu_async::{
    AsyncBuffer,
    AsyncBufferSlice,
};

use crate::gpu::Gpu;

#[derive(Debug)]
struct TensorBufferInner {
    buffer: AsyncBuffer,
    usage: TensorBufferUsage,
    map_count: AtomicUsize,
}

impl Drop for TensorBufferInner {
    fn drop(&mut self) {
        self.buffer.destroy();
    }
}

#[derive(Clone, Debug)]
pub struct TensorBuffer {
    inner: Arc<TensorBufferInner>,
}

impl TensorBuffer {
    /// Allocate a tensor buffer
    pub fn allocate(gpu: &Gpu, size: usize, usage: TensorBufferUsage, label: &str) -> Self {
        let size: BufferAddress = size.try_into().unwrap();

        let buffer = gpu.device().create_buffer(&BufferDescriptor {
            label: Some(label),
            size,
            usage: usage.into(),
            mapped_at_creation: false,
        });

        Self {
            inner: Arc::new(TensorBufferInner {
                buffer,
                usage,
                map_count: AtomicUsize::new(0),
            }),
        }
    }

    /// Allocate a tensor buffer and initially map it to memory. This mapping is
    /// writable.
    pub fn mapped(
        gpu: &Gpu,
        size: usize,
        usage: TensorBufferUsage,
        label: &str,
    ) -> (Self, MappedTensorBuffer) {
        let unpadded_size: BufferAddress = size.try_into().unwrap();
        // Valid vulkan usage is
        // 1. buffer size must be a multiple of COPY_BUFFER_ALIGNMENT.
        // 2. buffer size must be greater than 0.
        // Therefore we round the value up to the nearest multiple, and ensure it's at
        // least COPY_BUFFER_ALIGNMENT.
        let align_mask = wgpu::COPY_BUFFER_ALIGNMENT - 1;
        let padded_size =
            ((unpadded_size + align_mask) & !align_mask).max(wgpu::COPY_BUFFER_ALIGNMENT);

        let buffer = gpu.device().create_buffer(&BufferDescriptor {
            label: Some(label),
            size: padded_size,
            usage: usage.into(),
            mapped_at_creation: true,
        });

        let tensor_buffer = Self {
            inner: Arc::new(TensorBufferInner {
                buffer,
                usage,
                map_count: AtomicUsize::new(1),
            }),
        };

        let mapped_tensor_buffer = MappedTensorBuffer {
            inner: MappedTensorBufferInnerBuilder {
                buffer: tensor_buffer.clone(),
                map_write: true,
                slice_builder: move |buffer: &TensorBuffer| buffer.inner.buffer.slice(..),
            }
            .build(),
        };

        (tensor_buffer, mapped_tensor_buffer)
    }

    pub fn buffer(&self) -> &AsyncBuffer {
        &self.inner.buffer
    }

    pub fn as_binding(&self) -> BindingResource {
        if self.inner.usage != TensorBufferUsage::Compute {
            panic!(
                "can't bind tensor buffer to kernel. usage={:?}",
                self.inner.usage
            );
        }
        self.inner.buffer.as_entire_binding()
    }

    pub fn usage(&self) -> TensorBufferUsage {
        self.inner.usage
    }

    pub fn size(&self) -> BufferAddress {
        self.inner.buffer.size()
    }

    /// map buffer to host memory
    pub async fn map(&self, writable: bool) -> Result<MappedTensorBuffer, MapTensorBufferError> {
        self.usage().check_map(writable)?;

        let map_mode = writable.then_some(MapMode::Write).unwrap_or(MapMode::Read);

        let inner = MappedTensorBufferInnerAsyncTryBuilder {
            buffer: self.clone(),
            map_write: writable,
            slice_builder: move |buffer: &TensorBuffer| {
                Box::pin(async move {
                    let slice = buffer.inner.buffer.slice(..);
                    // we always tell wgpu to map the buffer. otherwise there could be a race
                    // condition between mapping and get_mapped_range.
                    slice.map_async(map_mode).await?;
                    Ok::<_, BufferAsyncError>(slice)
                })
            },
        }
        .try_build()
        .await?;

        self.inner.map_count.fetch_add(1, Ordering::Relaxed);

        Ok(MappedTensorBuffer { inner })
    }

    pub fn is_shared(&self) -> bool {
        //Arc::get_mut(&mut self.inner).is_none()
        Arc::strong_count(&self.inner) > 1
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MapTensorBufferError {
    #[error("buffer async error")]
    BufferAsync(#[from] BufferAsyncError),

    #[error("can't map the buffer readable: usage={usage:?}")]
    CantMapReadable { usage: TensorBufferUsage },

    #[error("can't map the buffer writable: usage={usage:?}")]
    CantMapWritable { usage: TensorBufferUsage },

    #[error("can't map the tensor buffer")]
    MappedTensorBufferView(#[from] MappedTensorBufferViewError),
}

/// helper struct to hold the buffer and the things referencing it.
/// we also need a separate struct, so we can define our own Drop impl for
/// `MappedTensorBuffer`
#[self_referencing]
struct MappedTensorBufferInner {
    // the tensor is contiguous and mapped to memory.
    buffer: TensorBuffer,

    map_write: bool,

    #[borrows(buffer)]
    #[covariant]
    slice: AsyncBufferSlice<'this>,
}

pub struct MappedTensorBuffer {
    inner: MappedTensorBufferInner,
}

impl MappedTensorBuffer {
    pub fn view<'a>(&'a self) -> Result<MappedTensorBufferView<'a>, MappedTensorBufferViewError> {
        let buffer = self.inner.borrow_buffer().clone();
        let buffer_view = self.inner.borrow_slice().get_mapped_range();
        Ok(MappedTensorBufferView {
            buffer,
            buffer_view,
        })
    }

    pub fn view_mut<'a>(
        &'a mut self,
    ) -> Result<MappedTensorBufferViewMut<'a>, MappedTensorBufferViewError> {
        if !*self.inner.borrow_map_write() {
            return Err(MappedTensorBufferViewError::NotMappedWritable);
        }

        let buffer = self.inner.borrow_buffer().clone();

        let buffer_view = self.inner.borrow_slice().get_mapped_range_mut();
        Ok(MappedTensorBufferViewMut {
            buffer,
            buffer_view,
        })
    }
}

impl Drop for MappedTensorBuffer {
    fn drop(&mut self) {
        let buffer = self.inner.borrow_buffer();
        if buffer.inner.map_count.fetch_sub(1, Ordering::Relaxed) == 1 {
            tracing::debug!("unmapping tensor buffer");
            buffer.inner.buffer.unmap()
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MappedTensorBufferViewError {
    #[error("tensor buffer is not mapped readable")]
    NotMappedReadable,

    #[error("tensor buffer is not mapped writable")]
    NotMappedWritable,

    #[error("a mut view to this tensor buffer already exists")]
    AlreadyMutBorrowed,
}

pub struct MappedTensorBufferView<'a> {
    buffer: TensorBuffer,
    buffer_view: BufferView<'a>,
}

impl<'a> Deref for MappedTensorBufferView<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.buffer_view
    }
}

pub struct MappedTensorBufferViewMut<'a> {
    buffer: TensorBuffer,
    buffer_view: BufferViewMut<'a>,
}

impl<'a> Deref for MappedTensorBufferViewMut<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.buffer_view
    }
}

impl<'a> DerefMut for MappedTensorBufferViewMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buffer_view
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TensorBufferUsage {
    Compute,
    CopyToHost,
}

impl TensorBufferUsage {
    fn check_map(&self, writable: bool) -> Result<(), MapTensorBufferError> {
        match (self, writable) {
            (Self::CopyToHost, false) => Ok(()),
            (_, false) => Err(MapTensorBufferError::CantMapReadable { usage: *self }),
            (_, true) => Err(MapTensorBufferError::CantMapWritable { usage: *self }),
        }
    }
}

impl Default for TensorBufferUsage {
    fn default() -> Self {
        Self::Compute
    }
}

impl From<TensorBufferUsage> for BufferUsages {
    fn from(value: TensorBufferUsage) -> Self {
        match value {
            TensorBufferUsage::Compute => BufferUsages::STORAGE | BufferUsages::COPY_SRC,
            TensorBufferUsage::CopyToHost => BufferUsages::MAP_READ | BufferUsages::COPY_DST,
        }
    }
}
