use std::marker::PhantomData;

use super::{
    KernelSignature,
    Map,
    MapKernel, MapSignature,
};
use crate::{
    element::{
        Element,
        Number,
    },
    error::{
        KernelError,
        ShapeMismatch,
    },
    kernel::{
        binding::{
            KernelBindingBuilder,
            KernelBindingDeclaration,
            KernelDeclaration,
            KernelParameterDeclaration,
        },
        TaskPartition,
    },
    tensor::{
        strider::contiguous_strides,
        Tensor,
    },
};

#[derive(Debug)]
pub struct TernaryArgs<'a, const D: usize, R: Element, A: Element, B: Element, C: Element> {
    pub result: &'a mut Tensor<D, R>,
    pub operand_1: &'a Tensor<D, A>,
    pub operand_2: &'a Tensor<D, B>,
    pub operand_3: &'a Tensor<D, C>,
}

pub struct TernarySignature<R: Element, A: Element, B: Element, C: Element>(
    PhantomData<(R, A, B, C)>,
);

impl<R: Element, A: Element, B: Element, C: Element> KernelSignature
    for TernarySignature<R, A, B, C>
{
    const DECLARATION: KernelDeclaration = KernelDeclaration {
        bindings: &[
            KernelBindingDeclaration::read_write::<R>("result"),
            KernelBindingDeclaration::read_only::<A>("operand_1"),
            KernelBindingDeclaration::read_only::<B>("operand_2"),
            KernelBindingDeclaration::read_only::<C>("operand_3"),
        ],
        parameters: &[
            KernelParameterDeclaration::shaped("op_strides"),
            KernelParameterDeclaration::shaped("op_shape"),
            KernelParameterDeclaration::int("result_offset"),
            KernelParameterDeclaration::shaped("result_strides"),
            KernelParameterDeclaration::int("operand_1_offset"),
            KernelParameterDeclaration::shaped("operand_1_strides"),
            KernelParameterDeclaration::int("operand_2_offset"),
            KernelParameterDeclaration::shaped("operand_2_strides"),
            KernelParameterDeclaration::int("operand_3_offset"),
            KernelParameterDeclaration::shaped("operand_3_strides"),
        ],
    };

    type Args<'a, const D: usize> = TernaryArgs<'a, D, R, A, B, C>;

    fn build_bind_group<'gpu, 'tensor, const D: usize>(
        args: Self::Args<'tensor, D>,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_binding("result", args.result)?;
        builder.add_binding("operand_1", args.operand_1)?;
        builder.add_binding("operand_2", args.operand_2)?;
        builder.add_binding("operand_3", args.operand_3)?;

        let result_strider = args.result.strider();
        let op_shape = result_strider.shape();
        builder.add_parameter("op_strides", contiguous_strides(&op_shape))?;
        builder.add_parameter("op_shape", op_shape)?;

        builder.add_parameter("result_offset", result_strider.offset())?;
        builder.add_parameter("result_strides", result_strider.strides())?;

        let operand_1_strider = args.operand_1.strider();
        builder.add_parameter("operand_1_offset", operand_1_strider.offset())?;
        builder.add_parameter("operand_1_strides", operand_1_strider.strides())?;

        let operand_2_strider = args.operand_2.strider();
        builder.add_parameter("operand_2_offset", operand_2_strider.offset())?;
        builder.add_parameter("operand_2_strides", operand_2_strider.strides())?;

        let operand_3_strider = args.operand_3.strider();
        builder.add_parameter("operand_3_offset", operand_3_strider.offset())?;
        builder.add_parameter("operand_3_strides", operand_3_strider.strides())?;

        Ok(())
    }

    fn task_partition<'a, const D: usize>(args: &Self::Args<'a, D>) -> TaskPartition {
        TaskPartition::for_result(&args.result)
    }
}

impl<R: Element, A: Element, B: Element, C: Element> MapSignature for TernarySignature<R, A, B, C> {
    const INPUTS: &'static [&'static str] = &["operand_1", "operand_2", "operand_3"];
    const OUTPUTS: &'static [&'static str] = &["result"];
}

impl<const D: usize, A: Element> Tensor<D, A> {
    pub async fn map_ternary_elementwise<
        M: Map<Signature = TernarySignature<R, A, B, C>>,
        B: Element,
        C: Element,
        R: Element,
    >(
        &self,
        operand_2: &Tensor<D, B>,
        operand_3: &Tensor<D, C>,
    ) -> Result<Tensor<D, R>, KernelError> {
        if self.shape() != operand_2.shape() {
            return Err(ShapeMismatch::new(&self.shape(), &operand_2.shape()).into());
        }
        if self.shape() != operand_3.shape() {
            return Err(ShapeMismatch::new(&self.shape(), &operand_3.shape()).into());
        }

        let mut result = Tensor::allocate(&self.gpu, self.shape());

        self.gpu
            .run_kernel::<D, MapKernel<M>>(TernaryArgs {
                result: &mut result,
                operand_1: self,
                operand_2,
                operand_3,
            })
            .await?;

        Ok(result)
    }
}

macro_rules! ternary_func_kernel {
    ($kernel:ident, $wsgl_func:ident) => {
        pub struct $kernel<T>(PhantomData<T>);

        impl<T: Element + Number> Map for $kernel<T> {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "let value_result = ",
                stringify!($wsgl_func),
                "(value_operand_1, value_operand_2, value_operand_3);"
            );
            type Signature = TernarySignature<T, T, T, T>;
        }
    };
}

macro_rules! ternary_tensor_impl {
    ($kernel:ident, $tensor_func:ident, $arg1_name:ident, $arg2_name:ident) => {
        impl<const D: usize, T: Element + Number> Tensor<D, T> {
            pub async fn $tensor_func(
                &self,
                $arg1_name: &Tensor<D, T>,
                $arg2_name: &Tensor<D, T>,
            ) -> Result<Tensor<D, T>, KernelError> {
                self.map_ternary_elementwise::<$kernel<T>, _, _, _>($arg1_name, $arg2_name)
                    .await
            }
        }
    };
}

macro_rules! ternary_func {
    ($kernel:ident, $wsgl_func:ident, $tensor_func:ident, $arg1_name:ident, $arg2_name:ident) => {
        ternary_func_kernel!($kernel, $wsgl_func);
        ternary_tensor_impl!($kernel, $tensor_func, $arg1_name, $arg2_name);
    };
}

ternary_func!(ElementwiseFusedMultiplyAdd, fma, fused_multiply_add, e2, e3);
ternary_func!(ElementwiseClamp, clamp, clamp, min, max);
ternary_func!(ElementwiseMix, mix, mix, e2, e3);
