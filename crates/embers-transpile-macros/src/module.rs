use darling::ast::NestedMeta;
use proc_macro2::TokenStream;
use quote::quote;
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
        // todo: include shader_std::prelude::*
        for item in items {
            output.push(crate::item::process_item(item, None)?);
        }

        let vis = &module.vis;
        let mod_token = &module.mod_token;
        let ident = &module.ident;
        Ok(quote!{
            #vis #mod_token #ident {
                #output
            }
        })
    }
    else {
        panic!("#[transpile] is only allowed on modules with content");
    }
}
