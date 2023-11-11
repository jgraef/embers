use std::marker::PhantomData;

use askama::Template;

use super::{
    unary::{
        UnaryArgs,
        UnarySignature,
    },
    BindGroupBuilder,
    BindingId,
    BindingTemplate,
    Kernel,
    KernelArgs,
    KernelSignature,
    KernelTemplateInfo,
};
use crate::{
    element::{
        Element,
        Number,
        WgslType,
        WgslValue,
    },
    error::KernelError,
    tensor::strider::Strider,
    Tensor,
};

pub struct Reduce<T> {
    _t: PhantomData<T>,
}

pub trait ReducerSignature: KernelSignature {
    const INPUT_NAME: &'static str;
    const INPUT_INDEX: usize;
}

pub trait ReduceKernel<S: ReducerSignature>: 'static {
    const LABEL: &'static str;

    const STATE_TYPE: WgslType;

    fn state_init() -> WgslValue;

    const FUNC: &'static str;
}

impl<K: ReduceKernel<S>, S: ReducerSignature> Kernel<S> for Reduce<K> {
    const LABEL: &'static str = <K as ReduceKernel<S>>::LABEL;
    type Template = ReduceKernelTemplate<'static>;

    fn template(info: KernelTemplateInfo<'static>) -> Self::Template {
        ReduceKernelTemplate {
            info,
            input_name: S::INPUT_NAME,
            input_index: S::INPUT_INDEX,
            state_type: K::STATE_TYPE,
            state_init: K::state_init(),
            func: K::FUNC,
        }
    }
}

#[derive(Debug, Template)]
#[template(path = "reduce.wgsl")]
pub struct ReduceKernelTemplate<'a> {
    info: KernelTemplateInfo<'a>,
    input_name: &'a str,
    input_index: usize,
    state_type: WgslType,
    state_init: WgslValue,
    func: &'a str,
}

pub struct FoldArgs<'a, const D: usize, R: Element, A: Element> {
    tensors: UnaryArgs<'a, D, R, A>,
    reducer: &'a Strider<D>,
}

impl<'a, const D: usize, R: Element, A: Element> KernelArgs<'a, D> for FoldArgs<'a, D, R, A> {
    fn create_bind_group(
        &'a self,
        bind_group_builder: &mut BindGroupBuilder<'a, '_, D>,
    ) -> Result<(), KernelError> {
        self.tensors.create_bind_group(bind_group_builder)?;
        bind_group_builder.add_reducer(&self.reducer);
        Ok(())
    }

    fn shape(&self) -> [usize; D] {
        // todo: make sure we actually reduce to this shape.
        self.tensors.result.shape()
    }
}

pub struct FoldSignature<R: Element, A: Element>(PhantomData<(R, A)>);

impl<R: Element, A: Element> ReducerSignature for FoldSignature<R, A> {
    const INPUT_NAME: &'static str = "operand";
    const INPUT_INDEX: usize = 1;
}

impl<R: Element, A: Element> KernelSignature for FoldSignature<R, A> {
    type Args<'a, const D: usize> = FoldArgs<'a, D, R, A>;

    fn binding_template() -> Vec<BindingTemplate<'static>> {
        UnarySignature::<R, A>::binding_template()
    }

    fn info_binding() -> BindingId {
        UnarySignature::<R, A>::info_binding()
    }
}

pub struct Sum;

impl<T: Element + Number> ReduceKernel<FoldSignature<T, T>> for Sum {
    const LABEL: &'static str = "Sum";

    const STATE_TYPE: WgslType = T::WGSL_TYPE;

    fn state_init() -> WgslValue {
        T::ZERO.into()
    }

    const FUNC: &'static str = "reduce_state += operand[reducer_offset];";
}

impl<const D: usize, T: Element + Number> Tensor<D, T> {
    pub async fn sum(&self, axis: usize) -> Result<Tensor<D, T>, KernelError> {
        let (reducer, reduced) = self.strider.reduce(axis);
        let mut result = Tensor::allocate(&self.gpu, reduced.shape());
        let args = FoldArgs {
            tensors: UnaryArgs {
                result: &mut result,
                operand: self,
            },
            reducer: &reducer,
        };
        self.gpu.run_kernel::<D, Reduce<Sum>, _>(&args).await?;

        // todo: later this will reduce the rank of the result tensor.
        Ok(result.with_strider(reduced))
    }
}
