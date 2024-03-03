#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

mod error;
mod llama;
mod tokenizer;

use std::path::PathBuf;

use color_eyre::eyre::Error;
use embers_core::file_formats::gguf::{
    metadata::MetadataValueType,
    Gguf,
};
use structopt::StructOpt;
use tokio::{
    fs::File,
    io::BufReader,
};
use tokio_util::compat::TokioAsyncReadCompatExt;

#[derive(Debug, StructOpt)]
pub enum Args {
    ShowGguf { path: PathBuf },
}

impl Args {
    pub async fn run(self) -> Result<(), Error> {
        match self {
            Args::ShowGguf { path } => {
                let reader = BufReader::new(File::open(&path).await?);
                let gguf = Gguf::open(reader.compat()).await?;

                let metadata = gguf.metadata();
                let tensor_infos = gguf.tensor_infos();

                println!("# Metadata: #{}", metadata.len());
                for (key, value) in metadata.iter() {
                    if value.ty() == MetadataValueType::Array {
                        println!(" - `{key}`: [...]");
                    }
                    else {
                        println!(" - `{key}`: `{value:?}`");
                    }
                }

                println!("# Tensor Infos: #{}", tensor_infos.len());
                for (key, tensor_info) in tensor_infos.iter() {
                    println!(
                        " - `{key}`: type={:?}, dimensions={:?}",
                        tensor_info.ty, tensor_info.dimensions
                    );
                }
            }
        }

        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    dotenvy::dotenv().ok();
    color_eyre::install()?;
    tracing_subscriber::fmt::init();

    let args = Args::from_args();
    args.run().await?;

    Ok(())
}
