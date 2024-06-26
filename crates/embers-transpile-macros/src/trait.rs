use darling::ast::NestedMeta;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    ImplItem,
    ItemImpl,
    ItemTrait,
    TraitItem,
};

use crate::{
    error::Error,
    function::{
        process_impl_function,
        SignatureTransform,
    },
    utils::{
        NameGen,
        TokenBuffer,
    },
};

pub fn process_trait(
    input: &ItemTrait,
    _attributes: Option<&[NestedMeta]>,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let vis = &input.vis;
    let ident = &input.ident;

    let mut output = TokenBuffer::default();

    for item in &input.items {
        match item {
            TraitItem::Fn(fn_item) => {
                let SignatureTransform { new_sig, .. } =
                    SignatureTransform::new(&fn_item.sig, name_gen, false)?;
                output.push(quote! { #new_sig; });
            }
            _ => output.push(quote! { #item }),
        }
    }

    let generic_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let super_traits = &input.supertraits;

    let output = quote! {
        #vis trait #ident <#generic_params> : #super_traits #where_clause {
            #output
        }
    };

    Ok(output)
}

pub fn process_impl(
    input: &ItemImpl,
    _attributes: Option<&[NestedMeta]>,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let trait_for = input.trait_.as_ref().map(|(not, trait_, for_token)| {
        assert!(not.is_none());
        quote! { #trait_ #for_token }
    });
    let self_ty = &input.self_ty;

    let mut output = TokenBuffer::default();

    for item in &input.items {
        match item {
            ImplItem::Fn(fn_item) => {
                output.push(process_impl_function(fn_item, None, name_gen)?);
            }
            _ => output.push(quote! { #item }),
        }
    }

    let generic_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;

    let output = quote! {
        impl <#generic_params> #trait_for #self_ty #where_clause {
            #output
        }
    };

    Ok(output)
}
