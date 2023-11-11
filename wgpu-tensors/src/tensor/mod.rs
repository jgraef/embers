pub mod buffer;
pub mod shape;
pub mod strider;
pub mod view;

use std::{
    marker::PhantomData,
    mem::size_of,
};

use derivative::Derivative;
use itertools::Itertools;
use wgpu::CommandEncoderDescriptor;

use self::{
    buffer::{
        TensorBuffer,
        TensorBufferUsage,
    },
    shape::Shape,
    strider::{
        Strider,
        TensorIndexIterator,
        TensorRangeBounds,
    },
    view::TensorView,
};
use crate::{
    element::Element,
    error::{
        InvalidAxis,
        SliceError,
    },
    gpu::Gpu,
};

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Tensor<const D: usize, T: Element> {
    #[derivative(Debug = "ignore")]
    pub(crate) gpu: Gpu,

    #[derivative(Debug = "ignore")]
    pub(crate) buffer: TensorBuffer,

    pub(crate) strider: Strider<D>,

    #[derivative(Debug = "ignore")]
    _t: PhantomData<T>,
}

impl<const D: usize, T: Element> Tensor<D, T> {
    pub(crate) fn allocate(gpu: &Gpu, shape: [usize; D]) -> Self {
        let size = shape.size();
        let label = "Tensor::allocate";
        let usage = TensorBufferUsage::Compute;

        let buffer = TensorBuffer::allocate(gpu, size * size_of::<T>(), usage, label);
        let strider = Strider::contiguous(shape);

        Self {
            gpu: gpu.clone(),
            buffer,
            strider,
            _t: PhantomData,
        }
    }

    fn allocate_and_initialize(
        gpu: &Gpu,
        shape: [usize; D],
        mut initializer: impl FnMut(&mut [T]),
    ) -> Self {
        let size = shape.size();
        let usage = TensorBufferUsage::Compute;
        let label = "Tensor::from_data";

        let buffer = if size == 0 {
            TensorBuffer::allocate(gpu, size, usage, label)
        }
        else {
            let (buffer, mut mapped) =
                TensorBuffer::mapped(gpu, size * size_of::<T>(), usage, label);

            let mut view = mapped.view_mut().unwrap();
            let copy_to = bytemuck::cast_slice_mut::<_, T>(&mut view);
            let copy_to = &mut copy_to[..size];
            initializer(copy_to);

            buffer
        };

        let strider = Strider::contiguous(shape);

        Self {
            gpu: gpu.clone(),
            buffer,
            strider,
            _t: PhantomData,
        }
    }

    pub fn from_data(gpu: &Gpu, shape: [usize; D], data: &[T]) -> Self {
        let size = shape.size();
        if size != data.len() {
            panic!(
                "shape size ({}) and data size ({}) don't match",
                size,
                data.len()
            );
        }

        Self::allocate_and_initialize(gpu, shape, |d| d.copy_from_slice(data))
    }

    pub fn from_closure(
        gpu: &Gpu,
        shape: [usize; D],
        mut f: impl FnMut(usize, [usize; D]) -> T,
    ) -> Self {
        Self::allocate_and_initialize(gpu, shape, |d| {
            TensorIndexIterator::new(shape)
                .zip(d)
                .enumerate()
                .for_each(|(i, (x, v))| *v = f(i, x))
        })
    }

    pub fn repeat(gpu: &Gpu, shape: [usize; D], value: T) -> Self {
        Self::allocate_and_initialize(gpu, shape, |d| d.iter_mut().for_each(|x| *x = value))
    }

    pub fn zeroes(gpu: &Gpu, shape: [usize; D]) -> Self {
        Self::repeat(gpu, shape, T::ZERO)
    }

    pub fn ones(gpu: &Gpu, shape: [usize; D]) -> Self {
        Self::repeat(gpu, shape, T::ONE)
    }

    pub fn diagonal(
        gpu: &Gpu,
        shape: [usize; D],
        value_on_diagonal: T,
        value_everywhere_else: T,
    ) -> Self {
        Self::from_closure(gpu, shape, |_, x| {
            x.iter()
                .all_equal()
                .then_some(value_on_diagonal)
                .unwrap_or(value_everywhere_else)
        })
    }

    pub fn strider(&self) -> &Strider<D> {
        &self.strider
    }

    pub fn shape(&self) -> [usize; D] {
        self.strider.shape()
    }

    pub fn size(&self) -> usize {
        self.strider.size()
    }

    pub fn actual_size(&self) -> usize {
        self.strider.actual_size()
    }

    pub(crate) fn with_strider<const E: usize>(&self, strider: Strider<E>) -> Tensor<E, T> {
        assert!(self.strider.size() <= strider.size());

        Tensor {
            gpu: self.gpu.clone(),
            buffer: self.buffer.clone(),
            strider,
            _t: PhantomData,
        }
    }

    async fn as_owned(&self) -> Tensor<D, T> {
        if self.buffer.is_shared() {
            self.id().await.expect("id() failed")
        }
        else {
            self.clone()
        }
    }

    pub async fn as_contiguous(&self) -> Tensor<D, T> {
        if self.strider.is_contiguous() {
            self.clone()
        }
        else {
            self.id().await.expect("id() failed")
        }
    }

    pub async fn ravel(&self) -> Tensor<1, T> {
        let result = self.as_contiguous().await;
        let strider = Strider::<1>::contiguous([result.strider.shape_size()]);
        result.with_strider(strider)
    }

    pub fn reshape<const E: usize>(&self, new_shape: [usize; E]) -> Option<Tensor<E, T>> {
        let strider = self.strider.reshape(new_shape)?;
        Some(self.with_strider(strider))
    }

    async fn copy_to_buffer(&self, destination: &TensorBuffer) {
        assert_eq!(destination.usage(), TensorBufferUsage::CopyToHost);
        assert!(self.strider.is_contiguous());

        let source_offset = (self.strider.offset() * size_of::<T>()).try_into().unwrap();
        let copy_size = (self.strider.size() * size_of::<T>()).try_into().unwrap();

        assert!(copy_size <= destination.size());

        let mut encoder = self
            .gpu
            .device()
            .create_command_encoder(&CommandEncoderDescriptor {
                label: Some("Tensor::copy_to_buffer"),
            });

        encoder.copy_buffer_to_buffer(
            &self.buffer.buffer(),
            source_offset,
            &destination.buffer(),
            0,
            copy_size,
        );

        self.gpu.queue().submit([encoder.finish()]).await;
    }

    pub async fn view(&self) -> TensorView<D, T> {
        let tensor = self.as_contiguous().await;

        let view_buffer = TensorBuffer::allocate(
            &tensor.gpu,
            tensor.strider.size() * size_of::<T>(),
            TensorBufferUsage::CopyToHost,
            "Tensor::view",
        );

        tensor.copy_to_buffer(&view_buffer).await;

        let view = TensorView::new(&view_buffer, tensor.strider).await.unwrap();

        view
    }

    pub fn slice(&self, bounds: impl TensorRangeBounds<D>) -> Result<Tensor<D, T>, SliceError> {
        Ok(self.with_strider(self.strider.slice(bounds)?))
    }

    pub fn flip_axis(&self, axis: &[usize]) -> Result<Tensor<D, T>, InvalidAxis> {
        Ok(self.with_strider(self.strider.flip_axis(axis)?))
    }

    pub fn transpose(&self, dim0: usize, dim1: usize) -> Result<Tensor<D, T>, InvalidAxis> {
        if dim0 >= D {
            return Err(InvalidAxis {
                axis: dim0,
                dimensions: D,
            });
        }
        if dim1 >= D {
            return Err(InvalidAxis {
                axis: dim1,
                dimensions: D,
            });
        }
        let mut permutation = [0; D];
        for i in 0..D {
            permutation[i] = i;
        }
        permutation[dim0] = dim1;
        permutation[dim1] = dim0;
        Ok(self.with_strider(self.strider.permute(permutation).unwrap()))
    }
}

impl<T: Element> Tensor<2, T> {
    pub fn eye(gpu: &Gpu, rows: usize, columns: usize) -> Self {
        let shape = [rows, columns];

        Self::diagonal(gpu, shape, T::ONE, T::ZERO)
    }
}
