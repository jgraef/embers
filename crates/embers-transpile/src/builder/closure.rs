use std::marker::PhantomData;

pub struct Closure<C, D> {
    _code: PhantomData<C>,
    data: D,
}
