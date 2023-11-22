use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    ItemTrait,
    TraitItem, ItemImpl, ImplItem,
};

use crate::{
    args::{TraitArgs, ImplArgs},
    error::Error,
    functions::{transform_signature_to_generator, process_impl_function},
    utils::{
        ident_to_literal,
        TokenBuffer,
    },
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
            _ => output.push(quote! { #item }),
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

pub fn process_impl(input: &ItemImpl, args: &ImplArgs) -> Result<TokenStream, Error> {
    let trait_for = input.trait_.as_ref().map(|(not, trait_, _)| {
        assert!(not.is_none());
        quote!{ #trait_ for }
    });
    let self_ty = &input.self_ty;

    let mut output = TokenBuffer::default();

    for item in &input.items {
        match item {
            ImplItem::Fn(fn_item) => {
                output.push(process_impl_function(fn_item, args)?);
            },
            _ => output.push(quote! { #item }),
        }
    }

    let output = quote! {
        impl #trait_for #self_ty {
            #output
        }
    };

    Ok(output)
}
