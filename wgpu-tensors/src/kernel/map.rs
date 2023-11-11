use std::marker::PhantomData;

use askama::Template;

use super::{
    Kernel,
    KernelSignature,
    KernelTemplateInfo,
};

pub struct Map<T> {
    _t: PhantomData<T>,
}

pub trait MapKernel<S: KernelSignature>: 'static {
    const LABEL: &'static str;
    const BODY: &'static str;
}

impl<K: MapKernel<S>, S: KernelSignature> Kernel<S> for Map<K> {
    const LABEL: &'static str = <K as MapKernel<S>>::LABEL;
    type Template = MapKernelTemplate<'static>;

    fn template(info: KernelTemplateInfo<'static>) -> Self::Template {
        MapKernelTemplate {
            info,
            body: K::BODY,
        }
    }
}

#[derive(Debug, Template)]
#[template(path = "map.wgsl")]
pub struct MapKernelTemplate<'a> {
    info: KernelTemplateInfo<'a>,
    body: &'a str,
}
