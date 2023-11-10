use std::marker::PhantomData;

use concat_idents::concat_idents;

use super::{
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
    error::{
        KernelError,
        ShapeMismatch,
    },
    tensor::Tensor,
    utils::max_rank,
};

#[derive(Debug)]
pub struct BinaryArgs<'a, const D: usize, R: Element, A: Element, B: Element> {
    pub result: &'a mut Tensor<D, R>,
    pub operand_a: &'a Tensor<D, A>,
    pub operand_b: &'a Tensor<D, B>,
}

impl<'a, const D: usize, R: Element, A: Element, B: Element> KernelArgs<'a, D>
    for BinaryArgs<'a, D, R, A, B>
{
    fn create_bind_group(
        &'a self,
        bind_group_builder: &mut BindGroupBuilder<'a, '_, D>,
    ) -> Result<(), KernelError> {
        bind_group_builder.add_tensor_binding(0, &self.result)?;
        bind_group_builder.add_tensor_binding(1, &self.operand_a)?;
        bind_group_builder.add_tensor_binding(2, &self.operand_b)?;
        bind_group_builder.add_info_binding(3);
        Ok(())
    }

    fn shape(&self) -> [usize; D] {
        self.result.shape()
    }
}

pub struct BinarySignature<R: Element, A: Element, B: Element>(PhantomData<(R, A, B)>);

impl<R: Element, A: Element, B: Element> KernelSignature for BinarySignature<R, A, B> {
    type Args<'a, const D: usize> = BinaryArgs<'a, D, R, A, B>;

    fn binding_template() -> Vec<BindingTemplate<'static>> {
        vec![
            BindingTemplate::read_write::<R>(0, "result"),
            BindingTemplate::read_only::<A>(1, "operand_a"),
            BindingTemplate::read_only::<B>(2, "operand_b"),
        ]
    }

    fn info_binding() -> super::BindingId {
        3
    }
}

impl<const D: usize, T1: Element> Tensor<D, T1> {
    async fn binary_elementwise<K: Kernel<BinarySignature<R, T1, T2>>, T2: Element, R: Element>(
        &self,
        other: &Tensor<D, T2>,
    ) -> Result<Tensor<D, R>, KernelError> {
        if self.shape() != other.shape() {
            return Err(ShapeMismatch::new(&self.shape(), &other.shape()).into());
        }

        let mut result = Tensor::allocate(&self.gpu, self.shape());

        self.gpu
            .run_kernel::<D, K, _>(&BinaryArgs {
                result: &mut result,
                operand_a: self,
                operand_b: other,
            })
            .await?;

        Ok(result)
    }

    async fn binary_elementwise_broadcast<
        const E: usize,
        K: Kernel<BinarySignature<R, T1, T2>>,
        T2: Element,
        R: Element,
    >(
        &self,
        other: &Tensor<E, T2>,
    ) -> Result<Tensor<{ max_rank(D, E) }, R>, KernelError>
    where
        [(); max_rank(D, E)]:, // ???
    {
        let (this, other) = broadcast_to_common_shape(self, other)
            .ok_or_else(|| KernelError::from(ShapeMismatch::new(&self.shape(), &other.shape())))?;
        Ok(this.binary_elementwise::<K, _, _>(&other).await?)
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
        pub struct $kernel;

        impl<T: Element + Number> Kernel<BinarySignature<T, T, T>> for $kernel {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "result[index_result] = ",
                stringify!($wsgl_func),
                "(operand_a[index_operand_a], operand_b[index_operand_b]);"
            );
        }
    };
}

macro_rules! binary_infix_kernel {
    ($kernel:ident, $op:tt) => {
        pub struct $kernel;

        impl<T: Element + Number> Kernel<BinarySignature<T, T, T>> for $kernel {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "result[index_result] = operand_a[index_operand_a] ",
                stringify!($op),
                " operand_b[index_operand_b];"
            );
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
                self.binary_elementwise::<$kernel, _, _>(other)
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
                    self.binary_elementwise_broadcast::<E, $kernel, _, _>(other)
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
binary_func!(ElementwiseMax, max);
binary_func!(ElementwiseMin, min);
