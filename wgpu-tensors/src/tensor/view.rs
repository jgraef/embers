use std::fmt::Debug;

use bytemuck::Pod;
use ouroboros::self_referencing;

use super::{
    buffer::{
        MapTensorBufferError,
        MappedTensorBuffer,
        MappedTensorBufferView,
    },
    strider::{
        Strider,
        TensorIndex,
    },
};
use crate::element::{
    block::{
        Block,
        DecodeFromBlock,
    },
    Element,
};

#[self_referencing]
struct TensorViewInner<T: Pod> {
    mapped_buffer: MappedTensorBuffer<T>,
    #[borrows(mapped_buffer)]
    #[covariant]
    view: MappedTensorBufferView<'this, T>,
}

pub struct TensorView<const D: usize, T: Element> {
    inner: TensorViewInner<T::Block>,
    strider: Strider<D>,
}

impl<const D: usize, T: Element + DecodeFromBlock<T::Block>> TensorView<D, T> {
    pub(crate) async fn new(
        mapped_buffer: MappedTensorBuffer<T::Block>,
        strider: Strider<D>,
    ) -> Result<Self, MapTensorBufferError> {
        assert!(strider.is_contiguous());
        Ok(Self {
            inner: TensorViewInnerTryBuilder {
                mapped_buffer,
                view_builder: |mapped_buffer| mapped_buffer.view(),
            }
            .try_build()?,
            strider,
        })
    }

    pub fn shape(&self) -> [usize; D] {
        self.strider.shape()
    }

    fn read(&self, index: usize) -> T {
        let (offset, i) = T::Block::buffer_index(index);
        let view = self.inner.borrow_view();
        let view = bytemuck::cast_slice(view);
        T::decode_from(&view[offset], i)
    }

    pub fn get(&self, index: impl TensorIndex<D>) -> T {
        self.read(self.strider.buffer_offset(index).unwrap())
    }

    pub fn iter(&self) -> TensorViewIterator<D, T> {
        TensorViewIterator {
            view: self,
            index: 0,
            num_elements: self.strider.shape_size(),
        }
    }

    pub fn to_vec(&self) -> Vec<T> {
        self.iter().collect()
    }
}

impl<const D: usize, T: Element + DecodeFromBlock<T::Block>> Debug for TensorView<D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

pub struct TensorViewIterator<'a, const D: usize, T: Element> {
    view: &'a TensorView<D, T>,
    index: usize,
    num_elements: usize,
}

impl<'a, const D: usize, T: Element + DecodeFromBlock<T::Block>> Iterator
    for TensorViewIterator<'a, D, T>
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        assert!(self.index <= self.num_elements);
        if self.index == self.num_elements {
            None
        }
        else {
            let value = self.view.read(self.index);
            self.index += 1;
            Some(value)
        }
    }
}
