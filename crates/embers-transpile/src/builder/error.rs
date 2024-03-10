use super::r#type::TypeHandle;

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
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
    #[error("expression is not constant")]
    NotConst,
    #[error("bad handle")]
    BadHandle,
    #[error("function ({name:?}) can't capture variables")]
    FunctionCantCapture { name: Option<String> },
    #[error("type not found: {name}")]
    TypeNotFound { name: String },
}
