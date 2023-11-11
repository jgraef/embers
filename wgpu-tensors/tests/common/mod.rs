use tokio::sync::OnceCell;
use wgpu_tensors::{
    element::Element,
    Gpu,
    Tensor,
};

static GPU: OnceCell<Gpu> = OnceCell::const_new();

pub async fn gpu() -> Gpu {
    let gpu = GPU
        .get_or_init(|| async { Gpu::new().await.unwrap() })
        .await;
    gpu.clone()
}

pub async fn tensor_to_vec<const D: usize, T: Element>(tensor: &Tensor<D, T>) -> Vec<T> {
    tensor.view().await.into_iter().copied().collect()
}
