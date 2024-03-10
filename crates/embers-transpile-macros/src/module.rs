use darling::{
    ast::NestedMeta,
    FromAttributes,
    FromMeta,
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Attribute,
    ItemMod,
};

use crate::{
    error::Error,
    utils::{
        NameGen,
        TokenBuffer,
    },
};

#[derive(Debug, FromAttributes)]
#[darling(attributes(transpile))]
pub struct ModAttributes {
    #[darling(flatten)]
    pub meta: ModMeta,
}

#[derive(Debug, FromMeta)]
pub struct ModMeta {
    #[darling(default)]
    pub no_prelude: bool,
    #[darling(default)]
    pub no_std: bool,
}

impl ModMeta {
    pub fn parse(
        nested_meta: Option<&[NestedMeta]>,
        item_attributes: &[Attribute],
    ) -> Result<Self, Error> {
        let meta = if let Some(nested_meta) = nested_meta {
            ModMeta::from_list(nested_meta)?
        }
        else {
            ModAttributes::from_attributes(item_attributes)?.meta
        };
        Ok(meta)
    }
}

pub fn process_module(
    module: &ItemMod,
    attributes: Option<&[NestedMeta]>,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let mut output = TokenBuffer::default();
    let args = ModMeta::parse(attributes, &module.attrs)?;

    if !args.no_prelude {
        output.push(quote! {
            use ::embers_transpile::shader_std::prelude::*;
        });
    }

    if !args.no_std {
        output.push(quote! {
            use ::embers_transpile::shader_std as shader_std;
        });
    }

    if let Some((_, items)) = &module.content {
        // todo: include shader_std::prelude::*
        for item in items {
            output.push(crate::item::process_item(item, None, name_gen)?);
        }

        let vis = &module.vis;
        let mod_token = &module.mod_token;
        let ident = &module.ident;
        Ok(quote! {
            #vis #mod_token #ident {
                #output
            }
        })
    }
    else {
        panic!("#[transpile] is only allowed on modules with content");
    }
}
