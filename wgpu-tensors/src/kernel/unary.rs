use std::marker::PhantomData;

use super::{
    map::{
        Map,
        MapKernel,
    },
    BindGroupBuilder,
    BindingTemplate,
    Kernel,
    KernelArgs,
    KernelSignature,
};
use crate::{
    element::{
        Element,
        Number,
    },
    error::KernelError,
    tensor::Tensor,
};

#[derive(Debug)]
pub struct UnaryArgs<'a, const D: usize, R: Element, A: Element> {
    pub result: &'a mut Tensor<D, R>,
    pub operand: &'a Tensor<D, A>,
}

impl<'a, const D: usize, R: Element, A: Element> KernelArgs<'a, D> for UnaryArgs<'a, D, R, A> {
    fn create_bind_group(
        &'a self,
        bind_group_builder: &mut BindGroupBuilder<'a, '_, D>,
    ) -> Result<(), KernelError> {
        bind_group_builder.add_tensor_binding(0, &self.result)?;
        bind_group_builder.add_tensor_binding(1, &self.operand)?;
        bind_group_builder.add_info_binding(2);
        Ok(())
    }

    fn shape(&self) -> [usize; D] {
        self.result.shape()
    }
}

pub struct UnarySignature<R: Element, A: Element>(PhantomData<(R, A)>);

impl<R: Element, A: Element> KernelSignature for UnarySignature<R, A> {
    type Args<'a, const D: usize> = UnaryArgs<'a, D, R, A>;

    fn binding_template() -> Vec<BindingTemplate<'static>> {
        vec![
            BindingTemplate::read_write::<R>(0, "result"),
            BindingTemplate::read_only::<A>(1, "operand"),
        ]
    }

    fn info_binding() -> super::BindingId {
        2
    }
}

impl<const D: usize, T: Element> Tensor<D, T> {
    async fn unary_elementwise<K: Kernel<UnarySignature<R, T>>, R: Element>(
        &self,
    ) -> Result<Tensor<D, R>, KernelError> {
        let mut result = Tensor::allocate(&self.gpu, self.shape());
        self.gpu
            .run_kernel::<D, K, _>(&UnaryArgs {
                result: &mut result,
                operand: self,
            })
            .await?;
        Ok(result)
    }
}

pub struct Identity;
impl<T: Element> MapKernel<UnarySignature<T, T>> for Identity {
    const LABEL: &'static str = "Identity";
    const BODY: &'static str = "result[index_result] = operand[index_operand];";
}

impl<const D: usize, T: Element> Tensor<D, T> {
    pub async fn id(&self) -> Result<Tensor<D, T>, KernelError> {
        self.unary_elementwise::<Map<Identity>, _>().await
    }
}

pub struct ElementwiseNegate;
impl<T: Element + Number> MapKernel<UnarySignature<T, T>> for ElementwiseNegate {
    const LABEL: &'static str = "ElementwiseNegate";
    const BODY: &'static str = "result[index_result] = -operand[index_operand];";
}

impl<const D: usize, T: Element + Number> Tensor<D, T> {
    pub async fn neg(&self) -> Result<Tensor<D, T>, KernelError> {
        self.unary_elementwise::<Map<ElementwiseNegate>, _>().await
    }
}

macro_rules! unary_func_kernel {
    ($kernel:ident, $wsgl_func:ident) => {
        pub struct $kernel;

        impl<T: Element + Number> MapKernel<UnarySignature<T, T>> for $kernel {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "result[index_result] = ",
                stringify!($wsgl_func),
                "(operand[index_operand]);"
            );
        }
    };
}

macro_rules! unary_tensor_impl {
    ($kernel:ident, $tensor_func:ident) => {
        impl<const D: usize, T: Element + Number> Tensor<D, T> {
            pub async fn $tensor_func(&self) -> Result<Tensor<D, T>, KernelError> {
                self.unary_elementwise::<Map<$kernel>, _>().await
            }
        }
    };
}

macro_rules! unary_func {
    ($kernel:ident, $wsgl_func:ident, $tensor_func:ident) => {
        unary_func_kernel!($kernel, $wsgl_func);
        unary_tensor_impl!($kernel, $tensor_func);
    };
    ($kernel:ident, $func:ident) => {
        unary_func!($kernel, $func, $func);
    };
}

unary_func!(ElementwiseDegrees, degrees);
unary_func!(ElementwiseRadians, radians);
unary_func!(ElementwiseCos, cos);
unary_func!(ElementwiseCosh, cosh);
unary_func!(ElementwiseAcos, acos);
unary_func!(ElementwiseAcosh, acosh);
unary_func!(ElementwiseSin, sin);
unary_func!(ElementwiseSinh, sinh);
unary_func!(ElementwiseAsin, asin);
unary_func!(ElementwiseAsinh, asinh);
unary_func!(ElementwiseTan, tan);
unary_func!(ElementwiseTanh, tanh);
unary_func!(ElementwiseAtan, atan);
unary_func!(ElementwiseAtanh, atanh);
unary_func!(ElementwiseAtan2, atan2);
unary_func!(ElementwiseExp, exp);
unary_func!(ElementwiseExp2, exp2);
unary_func!(ElementwiseLog, log);
unary_func!(ElementwiseLog2, log2);
unary_func!(ElementwiseSqrt, sqrt);
unary_func!(ElementwiseInverseSqrt, inverseSqrt, inverse_sqrt);
unary_func!(ElementwiseAbsolute, abs);
unary_func!(ElementwiseSignum, sign);
unary_func!(ElementwiseFractional, fract);
unary_func!(ElementwiseTruncate, trunc);
unary_func!(ElementwiseCeil, ceil);
unary_func!(ElementwiseFloor, floor);
unary_func!(ElementwiseRound, round);
unary_func!(ElementwiseSaturate, saturate);
