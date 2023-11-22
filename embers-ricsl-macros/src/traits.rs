use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    ItemTrait,
    TraitItem,
};

use crate::{
    error::Error,
    functions::transform_signature_to_generator,
    utils::{
        ident_to_literal,
        TokenBuffer,
    }, args::TraitArgs,
};

pub fn process_trait(input: &ItemTrait, args: &TraitArgs) -> Result<TokenStream, Error> {
    let vis = &input.vis;
    let ident = &input.ident;
    let ident_literal = ident_to_literal(&ident);

    let mut output = TokenBuffer::default();

    for item in &input.items {
        match item {
            TraitItem::Fn(fn_item) => {
                let (sig, ret) = transform_signature_to_generator(&fn_item.sig, args);
                output.push(quote! { #sig; });
            }
            item => output.push(quote! { #item }),
        }
    }

    let super_traits = &input.supertraits;

    let output = quote! {
        #vis trait #ident: #super_traits {
            #output
        }
    };

    Ok(output)
}
