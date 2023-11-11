use std::{marker::PhantomData, borrow::Cow};

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

pub trait Fold: 'static {
    const LABEL: &'static str;
    const APPLY: &'static str;
    const EPILOG: &'static str;

    fn state() -> Vec<StateVariable>;
}

#[derive(Debug)]
pub struct StateVariable {
    pub name: Cow<'static, str>,
    pub ty: WgslType,
    pub init: Cow<'static, str>,
}

impl<F: Fold, R: Element, A: Element> Kernel for FoldKernel<F, R, A> {
    const LABEL: &'static str = <F as Fold>::LABEL;
    type Template = FoldKernelTemplate;
    type Signature = FoldSignature<R, A>;

    fn template(info: KernelTemplateInfo) -> Self::Template {
        FoldKernelTemplate {
            info,
            state: F::state(),
            apply: F::APPLY,
            epilog: F::EPILOG,
        }
    }
}

#[derive(Debug, Template)]
#[template(path = "fold.wgsl")]
pub struct FoldKernelTemplate {
    info: KernelTemplateInfo,
    state: Vec<StateVariable>,
    apply: &'static str,
    epilog: &'static str,
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


impl<const D: usize, T: Element + Number> Tensor<D, T> {
    pub async fn fold<F: Fold, R: Element>(
        &self,
        axis: &[usize],
    ) -> Result<Tensor<D, R>, KernelError> {
        let ReduceResult { reducer, reduced } = self.strider.reduce(axis)?;

        let result = Tensor::allocate(&self.gpu, reduced.shape());

        let args = FoldArgs {
            result: &result,
            input: self,
            reducer: &reducer,
            reduced: &reduced,
        };

        tracing::debug!("args: {:#?}", args);

        self.gpu.run_kernel::<D, FoldKernel<F, R, T>>(args).await?;

        // todo: later this will reduce the rank of the result tensor.
        Ok(result.with_strider(reduced))
    }
}


macro_rules! fold_func_kernel {
    ($kernel:ident, $state:expr, $apply:expr, $epilog:expr) => {
        pub struct $kernel<T>(PhantomData<T>);

        impl<T: Element> Fold for $kernel<T> {
            const LABEL: &'static str = stringify!($kernel);
            const APPLY: &'static str = $apply;
            const EPILOG: &'static str = $epilog;
        
            fn state() -> Vec<StateVariable> {
                $state
            }
        }                
    };
}

fn element_to_wgsl_literal<T: Element>(x: T) -> String {
    let wgsl_value: WgslValue = x.into();
    wgsl_value.to_string()
}

macro_rules! fold_tensor_impl {
    ($kernel:ident, $tensor_func:ident) => {
        impl<const D: usize, T: Element + Number> Tensor<D, T> {
            pub async fn $tensor_func(&self, axis: &[usize]) -> Result<Tensor<D, T>, KernelError> {
                self.fold::<$kernel<T>, T>(axis).await
            }
        }
    };
}

macro_rules! fold_func {
    ($kernel:ident, $state:expr, $apply:expr, $epilog:expr, $tensor_func:ident) => {
        fold_func_kernel!($kernel, $state, $apply, $epilog);
        fold_tensor_impl!($kernel, $tensor_func);
    };
}

fold_func!(
    Sum,
    vec![StateVariable {
        name: "s_sum".into(),
        ty: T::WGSL_TYPE,
        init: element_to_wgsl_literal(T::ZERO).into(),
    }],
    "s_sum += x;",
    "result[result_offset] = s_sum;",
    sum
);

fold_func!(
    Product,
    vec![StateVariable {
        name: "s_product".into(),
        ty: T::WGSL_TYPE,
        init: element_to_wgsl_literal(T::ONE).into(),
    }],
    "s_product *= x;",
    "result[result_offset] = s_product;",
    product
);

fold_func!(
    Minimum,
    vec![StateVariable {
        name: "s_min".into(),
        ty: T::WGSL_TYPE,
        init: "input[input_offset]".into(),
    }],
    "if (x < s_min) { s_min = x; }",
    "result[result_offset] = s_min;",
    min
);

fold_func!(
    Maximum,
    vec![StateVariable {
        name: "s_max".into(),
        ty: T::WGSL_TYPE,
        init: "input[input_offset]".into(),
    }],
    "if (x < s_max) { s_max = x; }",
    "result[result_offset] = s_max;",
    max
);
