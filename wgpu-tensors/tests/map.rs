#![allow(dead_code)]

mod common;

use common::{
    gpu,
    tensor_to_vec,
};
use pretty_assertions::assert_eq;
use wgpu_tensors::Tensor;

fn cpu_unary_elementwise<T: Copy, R>(d: &[T], f: impl FnMut(T) -> R) -> Vec<R> {
    d.into_iter().copied().map(f).collect()
}

fn cpu_binary_elementwise<T: Copy, R>(d1: &[T], d2: &[T], mut f: impl FnMut(T, T) -> R) -> Vec<R> {
    d1.into_iter().zip(d2).map(|(&a, &b)| f(a, b)).collect()
}

const D1F: [f32; 9] = [2., 3., 5., 7., 11., 13., 17., 19., 23.];
const D1I: [i32; 9] = [2, 3, 5, 7, 11, 13, 17, 19, 23];
const D2F: [f32; 9] = [1., 2., 3., 4., 5., 6., 7., 8., 9.];
const D2I: [i32; 9] = [1, 2, 3, 4, 5, 6, 7, 8, 9];

macro_rules! test_unary_elementwise {
    ($name:ident, $tensor_method:ident, $cpu_func:expr, $d1:expr) => {
        #[tokio::test]
        async fn $name() {
            let gpu = gpu().await;

            let t1 = Tensor::from_iter(&gpu, [3, 3], $d1);

            let t2 = t1.$tensor_method().await.unwrap();

            assert_eq!(
                tensor_to_vec(&t2).await,
                cpu_unary_elementwise(&$d1, $cpu_func),
            )
        }
    };

    ($name:ident, $tensor_method:ident, $cpu_func:expr) => {
        test_unary_elementwise!($name, $tensor_method, $cpu_func, D1I);
    };
}

macro_rules! test_binary_elementwise {
    ($name:ident, $tensor_method:ident, $cpu_func:expr, $d1:expr, $d2:expr) => {
        #[tokio::test]
        async fn $name() {
            let gpu = gpu().await;

            let t1 = Tensor::from_iter(&gpu, [3, 3], $d1);
            let t2 = Tensor::from_iter(&gpu, [3, 3], $d2);

            let t3 = t1.$tensor_method(&t2).await.unwrap();

            assert_eq!(
                tensor_to_vec(&t3).await,
                cpu_binary_elementwise(&$d1, &$d2, $cpu_func),
            )
        }
    };

    ($name:ident, $tensor_method:ident, $cpu_func:expr) => {
        test_binary_elementwise!($name, $tensor_method, $cpu_func, D1I, D2I);
    };
}

test_unary_elementwise!(it_identities_elementwise, id, |a| a);
test_unary_elementwise!(it_negates_elementwise, neg, |a| -a);

test_binary_elementwise!(it_adds_elementwise, add, |a, b| a + b);
test_binary_elementwise!(it_subtracts_elementwise, sub, |a, b| a - b);
test_binary_elementwise!(it_multiplies_elementwise, mul, |a, b| a * b);
test_binary_elementwise!(it_divides_elementwise, div, |a, b| a / b);
test_binary_elementwise!(it_modulos_elementwise, modulo, |a, b| a % b);


test_binary_elementwise!(elementwise_eq, equal, |a, b| a == b);
test_binary_elementwise!(elementwise_ne, not_equal, |a, b| a != b);
test_binary_elementwise!(elementwise_lt, less_than, |a, b| a < b);
test_binary_elementwise!(elementwise_le, less_than_or_equal, |a, b| a <= b);
test_binary_elementwise!(elementwise_gt, greater_than, |a, b| a > b);
test_binary_elementwise!(elementwise_ge, greater_than_or_equal, |a, b| a >= b);


#[tokio::test]
async fn it_broadcasts_with_same_dim() {
    let gpu = gpu().await;

    let t1 = Tensor::from_iter(&gpu, [3, 3], D1I);
    let t2 = Tensor::from_iter(&gpu, [1, 3], [1, 2, 3]);
    const D2B: [i32; 9] = [1, 2, 3, 1, 2, 3, 1, 2, 3];

    let t3 = t1.add_broadcast(&t2).await.unwrap();

    assert_eq!(
        tensor_to_vec(&t3).await,
        cpu_binary_elementwise(&D1I, &D2B, |a, b| a + b),
    )
}

#[tokio::test]
async fn it_broadcasts_increasing_dim() {
    let gpu = gpu().await;

    let t1 = Tensor::from_iter(&gpu, [3, 3], D1I);
    let t2 = Tensor::from_iter(&gpu, [3], [1, 2, 3]);
    const D2B: [i32; 9] = [1, 1, 1, 2, 2, 2, 3, 3, 3];

    let t3 = t1.add_broadcast(&t2).await.unwrap();

    assert_eq!(
        tensor_to_vec(&t3).await,
        cpu_binary_elementwise(&D1I, &D2B, |a, b| a + b),
    )
}
