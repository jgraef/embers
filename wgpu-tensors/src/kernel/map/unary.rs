use std::marker::PhantomData;

use super::{
    MapKernel,
    MapSignature,
};
use crate::{
    element::{
        Element,
        Encode,
        Number,
    },
    error::KernelError,
    kernel::{
        binding::{
            KernelBindingBuilder,
            KernelBindingDeclaration,
            KernelDeclaration,
            KernelParameterDeclaration,
        },
        map::Map,
        KernelSignature,
        TaskPartition,
    },
    tensor::{
        strider::contiguous_strides,
        Tensor,
    },
};

#[derive(Debug)]
pub struct UnaryArgs<'a, const D: usize, R: Element, A: Element> {
    pub result: &'a mut Tensor<D, R>,
    pub operand: &'a Tensor<D, A>,
}

pub struct UnarySignature<R: Element, A: Element>(PhantomData<(R, A)>);

impl<R: Element, A: Element> KernelSignature for UnarySignature<R, A> {
    const DECLARATION: KernelDeclaration = KernelDeclaration {
        bindings: &[
            KernelBindingDeclaration::read_write::<R>("result"),
            KernelBindingDeclaration::read_only::<A>("operand"),
        ],
        parameters: &[
            KernelParameterDeclaration::array("op_strides"),
            KernelParameterDeclaration::array("op_shape"),
            KernelParameterDeclaration::int("result_offset"),
            KernelParameterDeclaration::array("result_strides"),
            KernelParameterDeclaration::int("operand_offset"),
            KernelParameterDeclaration::array("operand_strides"),
        ],
    };

    type Args<'a, const D: usize> = UnaryArgs<'a, D, R, A>;

    fn build_bind_group<'gpu, 'tensor, const D: usize>(
        args: Self::Args<'tensor, D>,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_binding("result", args.result)?;
        builder.add_binding("operand", args.operand)?;

        let result_strider = args.result.strider();
        let op_shape = result_strider.shape();
        builder.add_parameter("op_strides", contiguous_strides(&op_shape))?;
        builder.add_parameter("op_shape", op_shape)?;

        builder.add_parameter("result_offset", result_strider.offset())?;
        builder.add_parameter("result_strides", result_strider.strides())?;

        let operand_strider = args.operand.strider();
        builder.add_parameter("operand_offset", operand_strider.offset())?;
        builder.add_parameter("operand_strides", operand_strider.strides())?;

        Ok(())
    }

    fn task_partition<'a, const D: usize>(args: &Self::Args<'a, D>) -> TaskPartition {
        TaskPartition::for_result(&args.result)
    }
}

impl<R: Element, A: Element> MapSignature for UnarySignature<R, A> {
    const INPUTS: &'static [&'static str] = &["operand"];
    const OUTPUTS: &'static [&'static str] = &["result"];
}

impl<const D: usize, T: Element> Tensor<D, T> {
    pub(crate) async fn map_unary_elementwise<
        'a,
        M: Map<Signature = UnarySignature<R, T>>,
        R: Element,
    >(
        &self,
    ) -> Result<Tensor<D, R>, KernelError> {
        let mut result = Tensor::allocate(&self.gpu, self.shape());
        self.gpu
            .run_kernel::<D, MapKernel<M>>(UnaryArgs {
                result: &mut result,
                operand: self,
            })
            .await?;
        Ok(result)
    }
}

impl<const D: usize, T: Element> Tensor<D, T> {
    pub async fn id(&self) -> Result<Tensor<D, T>, KernelError> {
        struct Identity<T>(PhantomData<T>);
        impl<T: Element> Map for Identity<T> {
            const LABEL: &'static str = "id";
            const BODY: &'static str = "let value_result = value_operand;";
            type Signature = UnarySignature<T, T>;
        }

        self.map_unary_elementwise::<Identity<T>, T>().await
    }
}

impl<const D: usize, T: Element + Number> Tensor<D, T> {
    pub async fn neg(&self) -> Result<Tensor<D, T>, KernelError> {
        struct ElementwiseNegate<T>(PhantomData<T>);
        impl<T: Element + Number> Map for ElementwiseNegate<T> {
            const LABEL: &'static str = "neg";
            const BODY: &'static str = "let value_result = -value_operand;";
            type Signature = UnarySignature<T, T>;
        }

        self.map_unary_elementwise::<ElementwiseNegate<T>, T>()
            .await
    }
}

impl<const D: usize> Tensor<D, bool> {
    pub async fn not(&self) -> Result<Tensor<D, bool>, KernelError> {
        struct ElementwiseBoolNot;
        impl Map for ElementwiseBoolNot {
            const LABEL: &'static str = "not";
            const BODY: &'static str = "let value_result = ~value_operand;";
            type Signature = UnarySignature<bool, bool>;
            const INDEX_STEP: usize = <bool as Encode>::NUM_PACKED;
            const MAP_ENCODED: bool = true;
        }
        self.map_unary_elementwise::<ElementwiseBoolNot, bool>()
            .await
    }
}

macro_rules! unary_func {
    ($wgsl_func:ident, $tensor_func:ident) => {
        impl<const D: usize, T: Element + Number> Tensor<D, T> {
            pub async fn $tensor_func(&self) -> Result<Tensor<D, T>, KernelError> {
                struct FuncKernel<T>(PhantomData<T>);

                impl<T: Element + Number> Map for FuncKernel<T> {
                    const LABEL: &'static str = stringify!($tensor_func);
                    const BODY: &'static str = concat!(
                        "let value_result = ",
                        stringify!($wsgl_func),
                        "(value_operand);"
                    );
                    type Signature = UnarySignature<T, T>;
                }

                self.map_unary_elementwise::<FuncKernel<T>, T>().await
            }
        }
    };
    ($func:ident) => {
        unary_func!($func, $func);
    };
}

unary_func!(degrees);
unary_func!(radians);
unary_func!(cos);
unary_func!(cosh);
unary_func!(acos);
unary_func!(acosh);
unary_func!(sin);
unary_func!(sinh);
unary_func!(asin);
unary_func!(asinh);
unary_func!(tan);
unary_func!(tanh);
unary_func!(atan);
unary_func!(atanh);
unary_func!(atan2);
unary_func!(exp);
unary_func!(exp2);
unary_func!(log);
unary_func!(log2);
unary_func!(sqrt);
unary_func!(inverseSqrt, inverse_sqrt);
unary_func!(abs);
unary_func!(sign);
unary_func!(fract);
unary_func!(trunc);
unary_func!(ceil);
unary_func!(floor);
unary_func!(round);
unary_func!(saturate);
