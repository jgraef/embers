use std::marker::PhantomData;

use concat_idents::concat_idents;

use super::{
    Map,
    MapKernel,
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
        KernelSignature,
        TaskPartition,
    },
    tensor::{
        strider::contiguous_strides,
        Tensor,
    },
    utils::max_rank,
};

#[derive(Debug)]
pub struct BinaryArgs<'a, const D: usize, R: Element, A: Element, B: Element> {
    pub result: &'a mut Tensor<D, R>,
    pub operand_1: &'a Tensor<D, A>,
    pub operand_2: &'a Tensor<D, B>,
}

pub struct BinarySignature<R: Element, A: Element, B: Element>(PhantomData<(R, A, B)>);

impl<R: Element, A: Element, B: Element> KernelSignature for BinarySignature<R, A, B> {
    const DECLARATION: KernelDeclaration = KernelDeclaration {
        bindings: &[
            KernelBindingDeclaration::read_write::<R>("result"),
            KernelBindingDeclaration::read_only::<A>("operand_1"),
            KernelBindingDeclaration::read_only::<B>("operand_2"),
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
        ],
    };

    type Args<'a, const D: usize> = BinaryArgs<'a, D, R, A, B>;

    fn build_bind_group<'gpu, 'tensor, const D: usize>(
        args: Self::Args<'tensor, D>,
        builder: &mut KernelBindingBuilder<'gpu, 'tensor, D>,
    ) -> Result<(), KernelError> {
        builder.add_binding("result", args.result)?;
        builder.add_binding("operand_1", args.operand_1)?;
        builder.add_binding("operand_2", args.operand_2)?;

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

        Ok(())
    }

    fn task_partition<'a, const D: usize>(
        gpu: &crate::Gpu,
        args: &Self::Args<'a, D>,
    ) -> TaskPartition {
        TaskPartition::from_shape(gpu, args.result.shape())
    }
}

impl<const D: usize, A: Element> Tensor<D, A> {
    pub async fn binary_elementwise<
        K: Map<Signature = BinarySignature<R, A, B>>,
        B: Element,
        R: Element,
    >(
        &self,
        other: &Tensor<D, B>,
    ) -> Result<Tensor<D, R>, KernelError> {
        if self.shape() != other.shape() {
            return Err(ShapeMismatch::new(&self.shape(), &other.shape()).into());
        }

        let mut result = Tensor::allocate(&self.gpu, self.shape());

        self.gpu
            .run_kernel::<D, MapKernel<K>>(BinaryArgs {
                result: &mut result,
                operand_1: self,
                operand_2: other,
            })
            .await?;

        Ok(result)
    }

    async fn binary_elementwise_broadcast<
        const E: usize,
        M: Map<Signature = BinarySignature<R, A, B>>,
        B: Element,
        R: Element,
    >(
        &self,
        other: &Tensor<E, B>,
    ) -> Result<Tensor<{ max_rank(D, E) }, R>, KernelError>
    where
        [(); max_rank(D, E)]:, // ???
    {
        let (this, other) = broadcast_to_common_shape(self, other)
            .ok_or_else(|| KernelError::from(ShapeMismatch::new(&self.shape(), &other.shape())))?;
        Ok(this.binary_elementwise::<M, _, _>(&other).await?)
    }
}

pub fn broadcast_to_common_shape<const D: usize, const E: usize, T: Element, U: Element>(
    tensor1: &Tensor<D, T>,
    tensor2: &Tensor<E, U>,
) -> Option<(Tensor<{ max_rank(D, E) }, T>, Tensor<{ max_rank(D, E) }, U>)> {
    let common_shape = tensor1.strider.common_broadcast_shape(&tensor2.strider)?;

    let tensor1 = tensor1.with_strider(tensor1.strider.broadcast(common_shape).unwrap());
    let tensor2 = tensor2.with_strider(tensor2.strider.broadcast(common_shape).unwrap());

    Some((tensor1, tensor2))
}

macro_rules! binary_func_kernel {
    ($kernel:ident, $wsgl_func:ident) => {
        pub struct $kernel<T>(PhantomData<T>);

        impl<T: Element + Number> Map for $kernel<T> {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "result[index_result] = ",
                stringify!($wsgl_func),
                "(operand_1[index_operand_1], operand_1[index_operand_1]);"
            );
            type Signature = BinarySignature<T, T, T>;
        }
    };
}

macro_rules! binary_infix_kernel {
    ($kernel:ident, $op:tt) => {
        pub struct $kernel<T>(PhantomData<T>);

        impl<T: Element + Number> Map for $kernel<T> {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "result[index_result] = operand_1[index_operand_1] ",
                stringify!($op),
                " operand_2[index_operand_2];"
            );
            type Signature = BinarySignature<T, T, T>;
        }
    };
}

macro_rules! binary_tensor_impl {
    ($kernel:ident, $tensor_func:ident) => {
        impl<const D: usize, T: Element + Number> Tensor<D, T> {
            pub async fn $tensor_func(
                &self,
                other: &Tensor<D, T>,
            ) -> Result<Tensor<D, T>, KernelError>
            {
                self.binary_elementwise::<$kernel<T>, _, _>(other)
                    .await
            }

            concat_idents!(fn_name = $tensor_func, _broadcast {
                pub async fn fn_name<const E: usize>(
                    &self,
                    other: &Tensor<E, T>,
                ) -> Result<Tensor<{ max_rank(D, E) }, T>, KernelError>
                where
                    [(); max_rank(D, E)]:, // ???
                {
                    self.binary_elementwise_broadcast::<E, $kernel<T>, _, _>(other)
                        .await
                }
            });
        }
    };
}

macro_rules! binary_func {
    ($kernel:ident, $wsgl_func:ident, $tensor_func:ident) => {
        binary_func_kernel!($kernel, $wsgl_func);
        binary_tensor_impl!($kernel, $tensor_func);
    };

    ($kernel:ident, $func:ident) => {
        binary_func!($kernel, $func, $func);
    };
}

macro_rules! binary_infix {
    ($kernel:ident, $op:tt, $tensor_func:ident) => {
        binary_infix_kernel!($kernel, $op);
        binary_tensor_impl!($kernel, $tensor_func);
    };
}

binary_infix!(ElementwiseAddition, +, add);
binary_infix!(ElementwiseSubtraction, -, sub);
binary_infix!(ElementwiseMultiplication, *, mul);
binary_infix!(ElementwiseDivision, /, div);
binary_infix!(ElementwiseModulo, %, modulo);

binary_func!(ElementwisePower, pow);
binary_func!(ElementwiseStep, step);
binary_func!(ElementwiseMax, max, max_elementwise);
binary_func!(ElementwiseMin, min, min_elementwise);
