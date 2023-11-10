use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{
        Deref,
        Index,
    },
};

use ouroboros::self_referencing;

use super::{
    buffer::{
        MapTensorBufferError,
        MappedTensorBuffer,
        MappedTensorBufferView,
        TensorBuffer,
    },
    strider::{
        Strider,
        TensorIndex,
    },
};
use crate::element::Element;

pub struct TensorView<const D: usize, T: Element> {
    inner: TensorViewInner,
    strider: Strider<D>,
    _t: PhantomData<T>,
}

#[self_referencing]
struct TensorViewInner {
    mapped_buffer: MappedTensorBuffer,
    #[borrows(mapped_buffer)]
    #[covariant]
    view: MappedTensorBufferView<'this>,
}

impl<const D: usize, T: Element> TensorView<D, T> {
    pub(crate) async fn new(
        tensor_buffer: &TensorBuffer,
        strider: Strider<D>,
    ) -> Result<Self, MapTensorBufferError> {
        let mapped_buffer = tensor_buffer.map(false).await?;

        Ok(Self {
            inner: TensorViewInnerTryBuilder {
                mapped_buffer,
                view_builder: |mapped_buffer| mapped_buffer.view(),
            }
            .try_build()?,
            strider,
            _t: PhantomData,
        })
    }

    pub fn shape(&self) -> [usize; D] {
        self.strider.shape()
    }
}

impl<const D: usize, T: Element> Deref for TensorView<D, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        bytemuck::cast_slice(&self.inner.borrow_view())
    }
}

impl<const D: usize, T: Element, I: TensorIndex<D>> Index<I> for TensorView<D, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        let buffer_offset = self.strider.buffer_offset(index).unwrap();
        &self.deref()[buffer_offset]
    }
}

impl<const D: usize, T: Element> Debug for TensorView<D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.deref()).finish()
    }
}
