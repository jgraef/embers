use darling::FromMeta;

#[derive(Debug, FromMeta)]
pub struct FnArgs {
    #[darling(default)]
    pub entrypoint: bool,
}

#[derive(Debug, FromMeta)]
pub struct TraitArgs {}

#[derive(Debug, FromMeta, Default)]
pub struct StructArgs {}

#[derive(Debug, FromMeta)]
pub struct ImplArgs {}
