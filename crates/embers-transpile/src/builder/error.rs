use super::r#type::TypeHandle;

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    #[error("type {ty} doesn't have a naga type")]
    NoNagaType { ty: &'static str },
    #[error("type {ty:?} is not a function")]
    NotAFunction { ty: TypeHandle },
    #[error("type {ty:?} is not a naga type")]
    NotANagaType { ty: TypeHandle },
    #[error("let is unbound")]
    LetUnbound,
    #[error("invalid")]
    Invalid,
}
