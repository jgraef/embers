use std::{
    borrow::Cow,
    marker::PhantomData,
};

use askama::Template;

use super::{
    binding::{
        KernelBindingBuilder,
        KernelBindingDeclaration,
        KernelDeclaration,
        KernelParameterDeclaration,
    },
    Kernel,
    KernelSignature,
    KernelTemplateInfo,
    TaskPartition,
};
use crate::{
    element::{
        wgsl::{
            WgslLiteral,
            WgslType,
        },
        Element,
        Number,
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
    pub ty: Cow<'static, str>,
    pub init: Cow<'static, str>,
}

impl StateVariable {
    pub fn new<T: WgslType + WgslLiteral>(name: &'static str, init: T) -> Self {
        Self {
            name: name.into(),
            ty: T::TYPE_NAME.into(),
            init: init.format_literal(),
        }
    }
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
            KernelParameterDeclaration::array("reduced_strides"),
            KernelParameterDeclaration::array("reduced_shape"),
            KernelParameterDeclaration::array("reducer_strides"),
            KernelParameterDeclaration::array("reducer_shape"),
            KernelParameterDeclaration::int("result_offset"),
            KernelParameterDeclaration::array("result_strides"),
            KernelParameterDeclaration::int("input_offset"),
            KernelParameterDeclaration::array("input_strides"),
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

    fn task_partition<'a, const D: usize>(args: &Self::Args<'a, D>) -> TaskPartition {
        TaskPartition::for_result(&args.result)
    }
}

impl<const D: usize, T: Element + Number> Tensor<D, T> {
    pub(crate) async fn fold<F: Fold, R: Element>(
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

        impl<T: Element + Number + WgslType + WgslLiteral> Fold for $kernel<T> {
            const LABEL: &'static str = stringify!($kernel);
            const APPLY: &'static str = $apply;
            const EPILOG: &'static str = $epilog;

            fn state() -> Vec<StateVariable> {
                $state
            }
        }
    };
}

macro_rules! fold_tensor_impl {
    ($kernel:ident, $tensor_func:ident) => {
        impl<const D: usize, T: Element + Number + WgslType + WgslLiteral> Tensor<D, T> {
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
    vec![StateVariable::new("s_sum", T::ZERO)],
    "s_sum += value_input;",
    "let value_result = s_sum;",
    sum
);

fold_func!(
    Product,
    vec![StateVariable::new("s_product", T::ONE)],
    "s_product *= value_input;",
    "let value_result = s_product;",
    product
);

fold_func!(
    Minimum,
    vec![StateVariable {
        name: "s_min".into(),
        ty: T::TYPE_NAME.into(),
        init: "b_input_decode(input_index)".into(),
    }],
    "if (value_input < s_min) { s_min = value_input; }",
    "let value_result = s_min;",
    min
);

fold_func!(
    Maximum,
    vec![StateVariable {
        name: "s_max".into(),
        ty: T::TYPE_NAME.into(),
        init: "b_input_decode(input_index)".into(),
    }],
    "if (value_input > s_max) { s_max = value_input; }",
    "let value_result = s_max;",
    max
);
