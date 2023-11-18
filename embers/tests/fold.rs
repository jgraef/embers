mod common;

use common::{
    gpu,
    tensor_to_vec,
};
use pretty_assertions::assert_eq;
use embers::Tensor;

const D1I: [i32; 9] = [2, 3, 5, 7, 11, 13, 17, 19, 23];

#[tokio::test]
async fn test_sum() {
    let gpu = gpu().await;

    let t1 = Tensor::from_iter(&gpu, [3, 3], D1I);

    let t2 = t1.sum(&[1]).await.unwrap();

    assert_eq!(tensor_to_vec(&t2).await, vec![10, 31, 59])
}

#[tokio::test]
async fn test_product() {
    let gpu = gpu().await;

    let t1 = Tensor::from_iter(&gpu, [3, 3], D1I);

    let t2 = t1.product(&[1]).await.unwrap();

    assert_eq!(tensor_to_vec(&t2).await, vec![30, 1001, 7429])
}

#[tokio::test]
async fn test_min() {
    let gpu = gpu().await;

    let t1 = Tensor::from_iter(&gpu, [3, 3], D1I);

    let t2 = t1.min(&[1]).await.unwrap();

    assert_eq!(tensor_to_vec(&t2).await, vec![2, 7, 17])
}

#[tokio::test]
async fn test_max() {
    let gpu = gpu().await;

    let t1 = Tensor::from_iter(&gpu, [3, 3], D1I);

    let t2 = t1.max(&[1]).await.unwrap();

    assert_eq!(tensor_to_vec(&t2).await, vec![5, 13, 23])
}
