use std::{
    cmp::min,
    ops::{
        Bound,
        RangeBounds,
    },
};

use itertools::Itertools;

use super::shape::Shape;
use crate::{
    error::{
        BroadcastError,
        InvalidAxis,
        PermuteError,
        SliceError,
    },
    utils::max_rank,
};

#[derive(Clone, Debug)]
pub struct Strider<const D: usize> {
    shape: [usize; D],
    strides: [isize; D],
    offset: usize,
    shape_size: usize,
    actual_size: usize,
    is_contiguous: bool,
}

impl<const D: usize> Strider<D> {
    pub fn contiguous(shape: [usize; D]) -> Self {
        // we rather do this at runtime, or else we need to add `Assert<{ D> 0 }>:
        // IsTrue` everywhere, and in some places we can't.
        assert!(D > 0);

        assert!(shape.iter().all(|&s| s > 0));

        let strides = contiguous_strides(&shape);
        let size = shape.size();

        Self {
            shape,
            strides,
            offset: 0,
            shape_size: size,
            actual_size: size,
            is_contiguous: true,
        }
    }

    pub fn shape(&self) -> [usize; D] {
        self.shape
    }

    pub fn strides(&self) -> [isize; D] {
        self.strides
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn is_contiguous(&self) -> bool {
        self.is_contiguous
    }

    pub fn shape_size(&self) -> usize {
        self.shape_size
    }

    pub fn actual_size(&self) -> usize {
        self.actual_size
    }

    pub fn buffer_offset(&self, index: impl TensorIndex<D>) -> Result<usize, IndexOutOfBounds> {
        if D == 0 {
            return Err(IndexOutOfBounds::empty());
        }

        let index = index.as_slice();
        let mut buffer_offset = self.offset as isize;

        for (axis, (&x, (&stride, &shape))) in index
            .iter()
            .zip(self.strides.iter().zip(&self.shape))
            .enumerate()
        {
            if x >= shape {
                return Err(IndexOutOfBounds::new(axis, index, &self.shape));
            }

            buffer_offset += (x as isize) * stride;
        }

        Ok(buffer_offset
            .try_into()
            .expect("expected positive buffer index"))
    }

    pub fn is_valid_index(&self, index: &[usize; D]) -> Result<(), IndexOutOfBounds> {
        if D == 0 {
            return Err(IndexOutOfBounds::empty());
        }
        else {
            for (axis, (i, s)) in index.iter().zip(&self.shape).enumerate() {
                if i >= s {
                    return Err(IndexOutOfBounds::new(axis, index, &self.shape));
                }
            }
        }
        Ok(())
    }

    pub fn iter_indices(&self) -> TensorIndexIterator<D> {
        TensorIndexIterator::new(self.shape)
    }

    pub fn reshape<const E: usize>(&self, new_shape: [usize; E]) -> Option<Strider<E>> {
        assert_eq!(self.actual_size, new_shape.size());

        todo!();
    }

    pub fn permute(&self, permutation: [usize; D]) -> Result<Strider<D>, PermuteError> {
        for x in &permutation {
            if *x >= D {
                return Err(PermuteError::InvalidAxis {
                    permutation: permutation.to_vec(),
                    invalid_axis: InvalidAxis {
                        axis: *x,
                        dimensions: D,
                    },
                });
            }
        }

        if !permutation.iter().all_unique() {
            return Err(PermuteError::InvalidPermutation {
                permutation: permutation.to_vec(),
            });
        }

        let shape = permutation.map(|i| self.shape[i]);
        let strides = permutation.map(|i| self.strides[i]);

        Ok(Self {
            shape,
            strides,
            offset: self.offset,
            shape_size: self.shape_size,
            actual_size: self.actual_size,
            is_contiguous: self.is_contiguous,
        })
    }

    pub fn broadcast<const E: usize>(
        &self,
        expanded_shape: [usize; E],
    ) -> Result<Strider<E>, BroadcastError>
//where
    //    Assert<{ E >= D }>: IsTrue,
    {
        // it seems that unfortunately we have to do this check at runtime :'(
        assert!(E >= D && D > 0);

        let mut strides = [0isize; E];

        for i in 0..D {
            if self.shape[i] == expanded_shape[i] {
                strides[i] = self.strides[i];
            }
            else if self.shape[i] == 1 {
                // we can expand this axis
                // strides[i] = 0
            }
            else {
                return Err(BroadcastError {
                    axis: i,
                    shape: self.shape.to_vec(),
                    expanded_shape: expanded_shape.to_vec(),
                });
            }
        }

        Ok(Strider {
            shape: expanded_shape,
            strides,
            offset: self.offset,
            shape_size: expanded_shape.size(),
            actual_size: self.actual_size,
            is_contiguous: self.is_contiguous,
        })
    }

    pub fn common_broadcast_shape<const E: usize>(
        &self,
        other: &Strider<E>,
    ) -> Option<[usize; max_rank(D, E)]> {
        let mut common_shape = [1; max_rank(D, E)];

        for i in 0..common_shape.len() {
            let a = self.shape.get(i).copied().unwrap_or(1);
            let b = other.shape.get(i).copied().unwrap_or(1);

            if a == 1 || a == b {
                common_shape[i] = b;
            }
            else if b == 1 {
                common_shape[i] = a;
            }
            else {
                return None;
            }
        }

        Some(common_shape)
    }

    pub fn slice(&self, bounds: impl TensorRangeBounds<D>) -> Result<Self, SliceError> {
        let (start, shape) = bounds.apply(&self.shape)?;

        let offset = self.buffer_offset(start).expect("start index out of range");
        let actual_size = calculate_actual_size(&shape, &self.strides);

        Ok(Strider {
            shape,
            strides: self.strides,
            offset,
            shape_size: shape.size(),
            actual_size,
            is_contiguous: shape == self.shape && offset == self.offset,
        })
    }

    pub fn flip_axis(&self, axis: &[usize]) -> Result<Self, InvalidAxis> {
        let mut strides = self.strides;
        let mut start = [0usize; D];

        for &axis in axis {
            if axis >= D {
                return Err(InvalidAxis {
                    axis,
                    dimensions: D,
                });
            }

            strides[axis] *= -1;
            start[axis] = self.shape[axis];
        }

        let offset = self.buffer_offset(start).unwrap();

        Ok(Strider {
            shape: self.shape,
            strides,
            offset,
            shape_size: self.shape_size,
            actual_size: self.actual_size,
            is_contiguous: self.is_contiguous,
        })
    }
}

impl Strider<0> {
    pub fn empty() -> Self {
        Self {
            shape: [],
            strides: [],
            offset: 0,
            shape_size: 0,
            actual_size: 0,
            is_contiguous: true,
        }
    }
}

impl<const D: usize> Shape for Strider<D> {
    fn size(&self) -> usize {
        self.shape_size
    }

    fn is_empty(&self) -> bool {
        D == 0
    }
}

pub trait TensorIndex<const D: usize> {
    fn as_slice(&self) -> &[usize; D];
}

impl<const D: usize> TensorIndex<D> for [usize; D] {
    fn as_slice(&self) -> &[usize; D] {
        self
    }
}

impl<const D: usize> TensorIndex<D> for &[usize; D] {
    fn as_slice(&self) -> &[usize; D] {
        self
    }
}

#[derive(Debug, thiserror::Error)]
#[error("tensor index out of bounds: axis={axis}, index={index:?}, shape={shape:?}")]
pub struct IndexOutOfBounds {
    axis: usize,
    index: Vec<usize>,
    shape: Vec<usize>,
}

impl IndexOutOfBounds {
    pub fn empty() -> Self {
        Self {
            axis: 0,
            index: vec![],
            shape: vec![],
        }
    }

    pub fn new<const D: usize>(axis: usize, index: &[usize; D], shape: &[usize; D]) -> Self {
        Self {
            axis,
            index: index.to_vec(),
            shape: shape.to_vec(),
        }
    }
}

pub struct TensorIndexIterator<const D: usize> {
    shape: [usize; D],
    index: [usize; D],
    exhausted: bool,
}

impl<const D: usize> TensorIndexIterator<D> {
    pub fn new(shape: [usize; D]) -> Self {
        Self {
            shape,
            index: [0; D],
            exhausted: D == 0 || shape.iter().any(|&x| x == 0),
        }
    }
}

impl<const D: usize> Iterator for TensorIndexIterator<D> {
    type Item = [usize; D];

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            None
        }
        else {
            let result = self.index;

            for i in (0..D).rev() {
                self.index[i] += 1;
                if self.index[i] < self.shape[i] {
                    break;
                }
                self.index[i] = 0;
            }

            self.exhausted = self.index.iter().all(|&x| x == 0);

            Some(result)
        }
    }
}

pub fn contiguous_strides<const D: usize>(shape: &[usize; D]) -> [isize; D] {
    let mut strides = [1; D];
    if D > 1 {
        for i in (0..D - 1).rev() {
            let s: isize = shape[i + 1].try_into().unwrap();
            strides[i] = strides[i + 1] * s;
        }
    }
    strides
}
pub trait TensorRangeBounds<const D: usize> {
    fn start_bounds(&self) -> [Bound<&usize>; D];

    fn end_bounds(&self) -> [Bound<&usize>; D];

    fn apply(&self, shape: &[usize; D]) -> Result<([usize; D], [usize; D]), SliceError> {
        let start_bounds = self.start_bounds();
        let end_bounds = self.end_bounds();

        let mut start = [0usize; D];
        let mut new_shape = [0usize; D];

        for i in 0..D {
            start[i] = match start_bounds[i] {
                Bound::Included(&x) => x,
                Bound::Excluded(&x) => x + 1,
                Bound::Unbounded => 0,
            };
            let end = match end_bounds[i] {
                Bound::Included(&x) => min(x + 1, shape[i]),
                Bound::Excluded(&x) => min(x, shape[i]),
                Bound::Unbounded => shape[i],
            };

            new_shape[i] = end.checked_sub(start[i]).ok_or_else(|| {
                SliceError {
                    axis: i,
                    start: start[i],
                    end,
                }
            })?;
        }

        Ok((start, new_shape))
    }
}

impl<const D: usize, R: RangeBounds<usize>> TensorRangeBounds<D> for [R; D] {
    fn start_bounds(&self) -> [Bound<&usize>; D] {
        let mut bounds = [Bound::Unbounded; D];
        for i in 0..D {
            bounds[i] = self[i].start_bound();
        }
        bounds
    }

    fn end_bounds(&self) -> [Bound<&usize>; D] {
        let mut bounds = [Bound::Unbounded; D];
        for i in 0..D {
            bounds[i] = self[i].end_bound();
        }
        bounds
    }
}

// todo: turn into macro and define for a few tuples
impl<R1: RangeBounds<usize>, R2: RangeBounds<usize>> TensorRangeBounds<2> for (R1, R2) {
    fn start_bounds(&self) -> [Bound<&usize>; 2] {
        [self.0.start_bound(), self.1.start_bound()]
    }

    fn end_bounds(&self) -> [Bound<&usize>; 2] {
        [self.0.end_bound(), self.1.end_bound()]
    }
}

fn calculate_actual_size<const D: usize>(shape: &[usize; D], strides: &[isize; D]) -> usize {
    shape
        .iter()
        .zip(strides)
        .map(|(&shape, &stride)| (stride == 0).then_some(1).unwrap_or(shape))
        .product()
}
