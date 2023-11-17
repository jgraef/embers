#![allow(dead_code)]

mod common;

use common::{
    gpu,
    tensor_to_vec,
};
use pretty_assertions::assert_eq;
use wgpu_tensors::kernel::rand::{
    Standard,
    Uniform,
};

const SEED1: [u8; 16] = [
    94, 209, 73, 91, 163, 98, 5, 49, 167, 178, 165, 194, 64, 115, 219, 166,
];

#[tokio::test]
async fn it_generates_standard_random_u32s() {
    let gpu = gpu().await;

    let t = gpu
        .rand::<1, u32, _>([32], Standard, Some(SEED1))
        .await
        .unwrap();

    assert_eq!(
        tensor_to_vec(&t).await,
        &[
            216332392, 3602340821, 2672016190, 555621108, 3953752587, 2890258977, 1980773876,
            950834527, 3246388391, 2332709900, 1302771690, 376510211, 3624695484, 1658377958,
            728446541, 3976696243, 3084054810, 2020429981, 37335089, 3302233039, 2372367204,
            1463014193, 432943770, 2744816232, 1681247630, 784346405, 4036791512, 3106925683,
            1140477859, 93761417, 3458273142, 2432462559,
        ]
    );
}

#[tokio::test]
async fn it_generates_uniform_random_f32s() {
    let gpu = gpu().await;

    let t = gpu
        .rand::<1, f32, _>([32], Uniform { min: 0.0, max: 1.0 }, Some(SEED1))
        .await
        .unwrap();

    assert_eq!(
        tensor_to_vec(&t).await,
        &[
            0.05036881,
            0.83873534,
            0.62212723,
            0.12936562,
            0.92055476,
            0.67294085,
            0.46118486,
            0.22138341,
            0.7558587,
            0.54312634,
            0.30332518,
            0.087663114,
            0.8439402,
            0.3861212,
            0.16960467,
            0.92589676,
            0.71806246,
            0.470418,
            0.008692753,
            0.76886106,
            0.55235976,
            0.34063452,
            0.10080258,
            0.6390773,
            0.39144596,
            0.18261988,
            0.9398888,
            0.7233875,
            0.2655382,
            0.02183053,
            0.80519193,
            0.56635183,
        ]
    );
}