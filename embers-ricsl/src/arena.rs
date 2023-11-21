use std::marker::PhantomData;

#[derive(Clone, Debug)]
pub struct Arena<T> {
    items: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { items: vec![] }
    }
}

impl<T> Arena<T> {
    pub fn insert(&mut self, x: T) -> Handle<T> {
        let index = self.items.len();
        self.items.push(x);
        Handle {
            index,
            _t: PhantomData,
        }
    }

    pub fn get(&self, handle: Handle<T>) -> Option<&T> {
        self.items.get(handle.index)
    }

    pub fn get_mut(&mut self, handle: Handle<T>) -> Option<&mut T> {
        self.items.get_mut(handle.index)
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &T)> {
        self.items.iter().enumerate().map(|(i, x)| {
            (
                Handle {
                    index: i,
                    _t: PhantomData,
                },
                x,
            )
        })
    }

    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (Handle<T>, &mut T)> {
        self.items.iter_mut().enumerate().map(|(i, x)| {
            (
                Handle {
                    index: i,
                    _t: PhantomData,
                },
                x,
            )
        })
    }
}

#[derive(Debug)]
pub struct Handle<T> {
    index: usize,
    _t: PhantomData<T>,
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _t: PhantomData,
        }
    }
}

impl<T> Copy for Handle<T> {}
