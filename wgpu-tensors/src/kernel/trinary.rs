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
    error::{
        KernelError,
        ShapeMismatch,
    },
    tensor::Tensor,
};

#[derive(Debug)]
pub struct TrinaryArgs<'a, const D: usize, R: Element, A: Element, B: Element, C: Element> {
    pub result: &'a mut Tensor<D, R>,
    pub operand_a: &'a Tensor<D, A>,
    pub operand_b: &'a Tensor<D, B>,
    pub operand_c: &'a Tensor<D, C>,
}

impl<'a, const D: usize, R: Element, A: Element, B: Element, C: Element> KernelArgs<'a, D>
    for TrinaryArgs<'a, D, R, A, B, C>
{
    fn create_bind_group(
        &'a self,
        bind_group_builder: &mut BindGroupBuilder<'a, '_, D>,
    ) -> Result<(), KernelError> {
        bind_group_builder.add_tensor_binding(0, &self.result)?;
        bind_group_builder.add_tensor_binding(1, &self.operand_a)?;
        bind_group_builder.add_tensor_binding(2, &self.operand_b)?;
        bind_group_builder.add_tensor_binding(3, &self.operand_b)?;
        bind_group_builder.add_info_binding(4);
        Ok(())
    }

    fn shape(&self) -> [usize; D] {
        self.result.shape()
    }
}

pub struct TrinarySignature<R: Element, A: Element, B: Element, C: Element>(
    PhantomData<(R, A, B, C)>,
);

impl<R: Element, A: Element, B: Element, C: Element> KernelSignature
    for TrinarySignature<R, A, B, C>
{
    type Args<'a, const D: usize> = TrinaryArgs<'a, D, R, A, B, C>;

    fn binding_template() -> Vec<BindingTemplate<'static>> {
        vec![
            BindingTemplate::read_write::<R>(0, "result"),
            BindingTemplate::read_only::<A>(1, "operand_a"),
            BindingTemplate::read_only::<B>(2, "operand_b"),
            BindingTemplate::read_only::<B>(3, "operand_c"),
        ]
    }

    fn info_binding() -> super::BindingId {
        4
    }
}

impl<const D: usize, T1: Element> Tensor<D, T1> {
    async fn trinary_op<
        K: Kernel<TrinarySignature<R, T1, T2, T3>>,
        T2: Element,
        T3: Element,
        R: Element,
    >(
        &self,
        operand_b: &Tensor<D, T2>,
        operand_c: &Tensor<D, T3>,
    ) -> Result<Tensor<D, R>, KernelError> {
        if self.shape() != operand_b.shape() {
            return Err(ShapeMismatch::new(&self.shape(), &operand_b.shape()).into());
        }
        if self.shape() != operand_c.shape() {
            return Err(ShapeMismatch::new(&self.shape(), &operand_c.shape()).into());
        }

        let mut result = Tensor::allocate(&self.gpu, self.shape());

        self.gpu
            .run_kernel::<D, K, _>(&TrinaryArgs {
                result: &mut result,
                operand_a: self,
                operand_b,
                operand_c,
            })
            .await?;

        Ok(result)
    }
}

macro_rules! trinary_func_kernel {
    ($kernel:ident, $wsgl_func:ident) => {
        pub struct $kernel;

        impl<T: Element + Number> MapKernel<TrinarySignature<T, T, T, T>> for $kernel {
            const LABEL: &'static str = stringify!($kernel);
            const BODY: &'static str = concat!(
                "result[index_result] = ",
                stringify!($wsgl_func),
                "(operand_a[index_operand_a], operand_b[index_operand_b], operand_c[index_operand_c]);"
            );
        }
    };
}

macro_rules! trinary_tensor_impl {
    ($kernel:ident, $tensor_func:ident, $arg1_name:ident, $arg2_name:ident) => {
        impl<const D: usize, T: Element + Number> Tensor<D, T> {
            pub async fn $tensor_func(
                &self,
                $arg1_name: &Tensor<D, T>,
                $arg2_name: &Tensor<D, T>,
            ) -> Result<Tensor<D, T>, KernelError> {
                self.trinary_op::<Map<$kernel>, _, _, _>($arg1_name, $arg2_name)
                    .await
            }
        }
    };
}

macro_rules! trinary_func {
    ($kernel:ident, $wsgl_func:ident, $tensor_func:ident, $arg1_name:ident, $arg2_name:ident) => {
        trinary_func_kernel!($kernel, $wsgl_func);
        trinary_tensor_impl!($kernel, $tensor_func, $arg1_name, $arg2_name);
    };
}

trinary_func!(ElementwiseFusedMultiplyAdd, fma, fused_multiply_add, e2, e3);
trinary_func!(ElementwiseClamp, clamp, clamp, min, max);
trinary_func!(ElementwiseMix, mix, mix, e2, e3);
