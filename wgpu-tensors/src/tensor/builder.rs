use bytemuck::{
    Pod,
    Zeroable,
};
use ouroboros::self_referencing;

use super::{
    buffer::{
        MappedTensorBuffer,
        MappedTensorBufferViewMut,
        TensorBuffer,
        TensorBufferUsage,
    },
    shape::Shape,
    strider::{
        contiguous_strides,
        Strider,
    },
};
use crate::{
    element::{
        block::{
            Block,
            EncodeIntoBlock,
        },
        Element,
    },
    Gpu,
    Tensor,
};

struct EncodeBuffer<B> {
    buffer: B,
    i: usize,
}

impl<B: Block> Default for EncodeBuffer<B> {
    fn default() -> Self {
        Self {
            buffer: Zeroable::zeroed(),
            i: 0,
        }
    }
}

impl<B: Block> EncodeBuffer<B> {
    fn write<T: EncodeIntoBlock<B>>(&mut self, value: T) -> Option<B> {
        value.encode_into(&mut self.buffer, self.i);

        self.i += 1;
        if self.i == B::NUM_PACKED {
            let output = std::mem::replace(&mut self.buffer, Zeroable::zeroed());
            self.i = 0;
            Some(output)
        }
        else {
            None
        }
    }

    fn flush(&mut self) -> Option<B> {
        (self.i != 0).then(|| self.buffer)
    }
}

#[self_referencing]
struct TensorBuilderInner<T: Pod> {
    mapped_buffer: MappedTensorBuffer<T>,
    #[borrows(mut mapped_buffer)]
    #[covariant]
    view: MappedTensorBufferViewMut<'this, T>,
}

pub struct TensorBuilder<const D: usize, T: Element> {
    gpu: Gpu,
    tensor_buffer: TensorBuffer<T::Block>,
    inner: TensorBuilderInner<T::Block>,
    strides: [usize; D],
    shape: [usize; D],
    num_elements: usize,
    index: usize,
    encode_buffer: EncodeBuffer<T::Block>,
}

impl<const D: usize, T: Element> TensorBuilder<D, T> {
    pub fn new(gpu: &Gpu, shape: [usize; D], label: &str) -> Self {
        let num_elements = shape.size();
        let encoded_size = T::Block::encoded_size(num_elements);
        let usage = TensorBufferUsage::Compute;

        let strides = contiguous_strides(&shape).map(|x| x as usize);

        let (tensor_buffer, mapped_buffer) = TensorBuffer::mapped(gpu, encoded_size, usage, label);

        let inner = TensorBuilderInnerTryBuilder {
            mapped_buffer,
            view_builder: |mapped_buffer: &mut MappedTensorBuffer<T::Block>| {
                mapped_buffer.view_mut()
            },
        }
        .try_build()
        .unwrap();

        Self {
            gpu: gpu.clone(),
            tensor_buffer,
            inner,
            strides,
            shape,
            num_elements,
            index: 0,
            encode_buffer: Default::default(),
        }
    }

    pub fn is_full(&self) -> bool {
        assert!(self.index <= self.num_elements);
        self.index == self.num_elements
    }

    pub fn index(&self) -> Option<[usize; D]> {
        if self.is_full() {
            None
        }
        else {
            let mut index = [0; D];
            for i in 0..D {
                index[i] = self.index / self.strides[i] % self.shape[i];
            }
            Some(index)
        }
    }

    pub fn write_element(&mut self, value: T)
    where
        T: EncodeIntoBlock<T::Block>,
    {
        if self.is_full() {
            panic!("tensor buffer is full");
        }

        if let Some(block) = self.encode_buffer.write(value) {
            let (offset, i) = T::Block::buffer_index(self.index);
            assert_eq!(i, 0);
            self.inner.with_view_mut(|view| view[offset] = block);
        }

        self.index += 1;
    }

    pub fn write_block(&mut self, block: T::Block) {
        if self.is_full() {
            panic!("tensor buffer is full");
        }

        let (offset, i) = T::Block::buffer_index(self.index);
        assert_eq!(i, 0, "packed_buffer is not empty");
        self.inner.with_view_mut(|view| view[offset] = block);

        self.index += T::Block::NUM_PACKED;
    }

    pub fn build(mut self) -> Tensor<D, T> {
        if let Some(block) = self.encode_buffer.flush() {
            let (offset, _) = T::Block::buffer_index(self.index);
            self.inner.with_view_mut(|view| view[offset] = block);
        }

        if !self.is_full() {
            panic!("tensor builder hasn't been filled yet.")
        }

        Tensor::new(
            self.gpu,
            self.tensor_buffer,
            Strider::contiguous(self.shape),
        )
    }
}
