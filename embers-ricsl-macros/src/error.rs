#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("syn error")]
    Syn(#[from] syn::Error),
}
