use serde::Deserialize;
use wgpu_tensors::file_formats::gguf::metadata::{
    General,
    Tokenizer,
};

#[derive(Debug, Deserialize)]
pub struct Metadata {
    pub general: General,
    pub llama: LLamaMetadata,
    pub tokenizer: Tokenizer,
}

#[derive(Debug, Deserialize)]
pub struct LLamaMetadata {
    pub attention: Attention,
    pub block_count: u32,
    pub context_length: u32,
    pub embedding_length: u32,
    pub feed_forward_length: u32,
    pub rope: Rope,
}

#[derive(Debug, Deserialize)]
pub struct Attention {
    pub head_count: u32,
    pub head_count_kv: Option<u32>,
    pub layer_norm_rms_epsilon: f32,
}

#[derive(Debug, Deserialize)]
pub struct Rope {
    pub dimension_count: u32,
}
