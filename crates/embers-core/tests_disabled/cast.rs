#![allow(dead_code)]

mod common;

use common::{
    gpu,
    tensor_to_vec,
};
use embers_core::Tensor;
use pretty_assertions::assert_eq;

const D2F: [f32; 9] = [1., 2., 3., 4., 5., 6., 7., 8., 9.];
const D2I: [i32; 9] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const D3I: [i32; 9] = [1, 2, 0, -1, 0, 2, 1, 1, 0];
const D3B: [bool; 9] = [true, true, false, true, false, true, true, true, false];

macro_rules! test_cast {
    ($name:ident, $from_data:expr, $to_ty:ident, $to_data:expr) => {
        #[tokio::test]
        async fn $name() {
            let gpu = gpu().await;

            let t1 = Tensor::from_iter(&gpu, [3, 3], $from_data);

            let t2 = t1.cast::<$to_ty>().await.unwrap();

            assert_eq!(tensor_to_vec(&t2).await, &$to_data,)
        }
    };
}

test_cast!(it_casts_i32_to_f32, D2I, f32, D2F);
test_cast!(it_casts_f32_to_i32, D2F, i32, D2I);
//test_cast!(it_casts_i32_to_bool, D3I, bool, D3B);
