pub mod binary;
pub mod ternary;
pub mod unary;

use std::marker::PhantomData;

use askama::Template;

use super::{
    binding::KernelParameterType,
    Kernel,
    KernelSignature,
    KernelTemplateInfo,
};

pub struct MapKernel<M> {
    _m: PhantomData<M>,
}

pub trait Map: 'static {
    const LABEL: &'static str;
    const BODY: &'static str;
    type Signature: KernelSignature;
}

impl<M: Map> Kernel for MapKernel<M> {
    const LABEL: &'static str = <M as Map>::LABEL;
    type Template = MapKernelTemplate;
    type Signature = M::Signature;

    fn template(info: KernelTemplateInfo) -> Self::Template {
        MapKernelTemplate {
            info,
            body: M::BODY,
        }
    }
}

#[derive(Debug, Template)]
#[template(path = "map.wgsl")]
pub struct MapKernelTemplate {
    info: KernelTemplateInfo,
    body: &'static str,
}
