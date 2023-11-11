use color_eyre::eyre::Error;
use wgpu_tensors::{
    gpu::Gpu,
    tensor::Tensor,
};

#[tokio::main]
async fn main() -> Result<(), Error> {
    dotenvy::dotenv().ok();
    color_eyre::install()?;
    tracing_subscriber::fmt::init();

    let gpu = Gpu::new().await?;
    let t1 = Tensor::from_data(&gpu, [2, 2], &[1, 2, 3, 4]);
    //let t2 = Tensor::from_data(&gpu, [1], &[2]);
    //let t2 = Tensor::from_data(&gpu, [2, 2], &[2, 2, 2, 2]);
    //let t3 = t1.add_broadcast(&t2).await?;
    //let t3 = t1.neg().await?;
    let t3 = t1.sum(&[0]).await?;

    let view = t3.view().await;
    tracing::debug!("{:?}", view);

    Ok(())
}
