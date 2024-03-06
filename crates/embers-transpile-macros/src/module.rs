use darling::ast::NestedMeta;
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::ItemMod;

use crate::{
    error::Error,
    utils::TokenBuffer,
};

pub fn process_module(
    module: &ItemMod,
    _attributes: Option<&[NestedMeta]>,
) -> Result<TokenStream, Error> {
    let mut output = TokenBuffer::default();

    if let Some((_, items)) = &module.content {
        for item in items {
            output.push(crate::item::process_item(item, None)?);
        }
        Ok(output.into_token_stream())
    }
    else {
        panic!("#[transpile] is only allowed on modules with content");
    }
}
