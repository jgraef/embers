pub mod binary;
pub mod ternary;
pub mod unary;
pub mod cast;

use std::marker::PhantomData;

use askama::Template;

use super::{
    Kernel,
    KernelSignature,
    KernelTemplateInfo,
};

pub struct MapKernel<M>(PhantomData<M>);

pub trait MapSignature: KernelSignature {
    const INPUTS: &'static [&'static str];
    const OUTPUTS: &'static [&'static str];
}

pub trait Map: 'static {
    const LABEL: &'static str;
    const BODY: &'static str;
    const INDEX_STEP: usize = 1;
    const MAP_ENCODED: bool = false;
    type Signature: MapSignature;
}

impl<M: Map> Kernel for MapKernel<M> {
    const LABEL: &'static str = <M as Map>::LABEL;
    type Template = MapKernelTemplate;
    type Signature = M::Signature;

    fn template(info: KernelTemplateInfo) -> Self::Template {
        MapKernelTemplate {
            info,
            inputs: M::Signature::INPUTS,
            outputs: M::Signature::OUTPUTS,
            body: M::BODY,
            index_step: M::INDEX_STEP,
            map_encoded: M::MAP_ENCODED,
        }
    }
}

#[derive(Debug, Template)]
#[template(path = "map.wgsl")]
pub struct MapKernelTemplate {
    info: KernelTemplateInfo,
    inputs: &'static [&'static str],
    outputs: &'static [&'static str],
    body: &'static str,
    index_step: usize,
    map_encoded: bool,
}
