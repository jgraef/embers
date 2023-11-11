use std::marker::PhantomData;

use askama::Template;

use super::{
    binding::{
        KernelBindingBuilder,
        KernelBindingDeclaration,
        KernelDeclaration,
        KernelParameterDeclaration,
        KernelParameterType,
    },
    Kernel,
    KernelSignature,
    KernelTemplateInfo,
    TaskPartition,
};
use crate::{
    element::{
        Element,
        Number,
        WgslType,
        WgslValue,
    },
    error::KernelError,
    tensor::strider::{
        ReduceResult,
        Strider,
    },
    Tensor,
};

pub struct FoldKernel<T, R, A> {
    _t: PhantomData<(T, R, A)>,
}

pub trait Fold<R: Element, A: Element>: 'static {
    const LABEL: &'static str;

    const STATE_TYPE: WgslType;

    fn state_init() -> WgslValue;

    const APPLY: &'static str;
}

impl<F: Fold<R, A>, R: Element, A: Element> Kernel for FoldKernel<F, R, A> {
    const LABEL: &'static str = <F as Fold<R, A>>::LABEL;
    type Template = FoldKernelTemplate;
    type Signature = FoldSignature<R, A>;

    fn template(info: KernelTemplateInfo) -> Self::Template {
        FoldKernelTemplate {
            info,
            state: StateTemplate {
                init: F::state_init(),
                ty: F::STATE_TYPE,
            },
            apply: F::APPLY,
        }
    }
}

#[derive(Debug, Template)]
#[template(path = "fold.wgsl")]
pub struct FoldKernelTemplate {
    info: KernelTemplateInfo,
    state: StateTemplate,
    apply: &'static str,
}

#[derive(Debug)]
pub struct StateTemplate {
    init: WgslValue,
    ty: WgslType,
}

pub struct FoldSignature<R: Element, A: Element>(PhantomData<(R, A)>);

#[derive(Debug)]
pub struct FoldArgs<'a, const D: usize, R: Element, A: Element> {
    result: &'a Tensor<D, R>,
    input: &'a Tensor<D, A>,
    reducer: &'a Strider<D>,
    reduced: &'a Strider<D>,
}

impl<R: Element, A: Element> KernelSignature for FoldSignature<R, A> {
    const DECLARATION: KernelDeclaration = KernelDeclaration {
        bindings: &[
            KernelBindingDeclaration::read_write::<R>("result"),
            KernelBindingDeclaration::read_only::<A>("input"),
        ],
        parameters: &[
            KernelParameterDeclaration::shaped("reduced_strides"),
            KernelParameterDeclaration::shaped("reduced_shape"),
            KernelParameterDeclaration::shaped("reducer_strides"),
            KernelParameterDeclaration::shaped("reducer_shape"),
            KernelParameterDeclaration::int("result_offset"),
            KernelParameterDeclaration::shaped("result_strides"),
            KernelParameterDeclaration::int("input_offset"),
            KernelParameterDeclaration::shaped("input_strides"),
        ],
    };

    type Args<'a, const D: usize> = FoldArgs<'a, D, R, A>;

    fn build_bind_group<'gpu, 'tensor, const D: usize>(
        args: Self::Args<'tensor, D>,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_binding("result", args.result)?;
        builder.add_binding("input", args.input)?;

        builder.add_parameter("reduced_strides", args.reduced.strides())?;
        builder.add_parameter("reduced_shape", args.reduced.shape())?;

        builder.add_parameter("reducer_strides", args.reducer.strides())?;
        builder.add_parameter("reducer_shape", args.reducer.shape())?;

        builder.add_parameter("result_offset", args.result.strider.offset())?;
        builder.add_parameter("result_strides", args.result.strider.strides())?;

        builder.add_parameter("input_offset", args.input.strider.offset())?;
        builder.add_parameter("input_strides", args.input.strider.strides())?;

        Ok(())
    }

    fn task_partition<'a, const D: usize>(
        gpu: &crate::Gpu,
        args: &Self::Args<'a, D>,
    ) -> TaskPartition {
        TaskPartition::from_shape(gpu, args.reduced.shape())
    }
}

pub struct Sum;

impl<T: Element> Fold<T, T> for Sum {
    const LABEL: &'static str = "Sum";

    const STATE_TYPE: WgslType = T::WGSL_TYPE;

    fn state_init() -> WgslValue {
        T::ZERO.into()
    }

    const APPLY: &'static str = "reducer_state += x;";
}

impl<const D: usize, T: Element + Number> Tensor<D, T> {
    pub async fn sum(&self, axis: &[usize]) -> Result<Tensor<D, T>, KernelError> {
        let ReduceResult { reducer, reduced } = self.strider.reduce(axis)?;

        let result = Tensor::allocate(&self.gpu, reduced.shape());

        let args = FoldArgs {
            result: &result,
            input: self,
            reducer: &reducer,
            reduced: &reduced,
        };

        tracing::debug!("args: {:#?}", args);

        self.gpu
            .run_kernel::<D, FoldKernel<Sum, T, T>>(args)
            .await?;

        // todo: later this will reduce the rank of the result tensor.
        Ok(result.with_strider(reduced))
    }
}
