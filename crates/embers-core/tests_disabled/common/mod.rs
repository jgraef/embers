use embers_core::{
    element::{
        block::DecodeFromBlock,
        Element,
    },
    Gpu,
    Tensor,
};
use tokio::sync::OnceCell;

static GPU: OnceCell<Gpu> = OnceCell::const_new();

pub async fn gpu() -> Gpu {
    let gpu = GPU
        .get_or_init(|| async { Gpu::new().await.unwrap() })
        .await;
    gpu.clone()
}

pub async fn tensor_to_vec<const D: usize, T: Element + DecodeFromBlock<T::Block>>(
    tensor: &Tensor<D, T>,
) -> Vec<T> {
    tensor.view().await.iter().collect()
}
