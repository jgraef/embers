use darling::{
    ast::NestedMeta,
    FromAttributes,
    FromMeta,
};
use syn::Attribute;

use crate::error::Error;

#[derive(Debug, FromAttributes)]
#[darling(attributes(transpile))]
pub struct FnAttributes {
    #[darling(flatten)]
    pub meta: FnMeta,
}

#[derive(Debug, FromMeta)]
pub struct FnMeta {
    #[darling(default)]
    pub entrypoint: bool,
    #[darling(default)]
    pub inline: bool,
}

impl FnMeta {
    pub fn parse(
        nested_meta: Option<&[NestedMeta]>,
        item_attributes: &[Attribute],
    ) -> Result<Self, Error> {
        let meta = if let Some(nested_meta) = nested_meta {
            FnMeta::from_list(nested_meta)?
        }
        else {
            FnAttributes::from_attributes(item_attributes)?.meta
        };
        Ok(meta)
    }
}
