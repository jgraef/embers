#![allow(dead_code)]

mod closure;
mod error;
mod expression;
mod function;
mod item;
mod module;
mod r#struct;
mod r#trait;
mod utils;

use darling::ast::NestedMeta;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_error::proc_macro_error;
use quote::{
    quote,
    ToTokens,
};
use syn::{
    parse_macro_input,
    Ident,
    Item,
    LitInt,
};

use crate::{
    error::Error,
    utils::{
        NameGen,
        TokenBuffer,
    },
};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn transpile(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let attributes = match NestedMeta::parse_meta_list(attrs.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(darling::Error::from(e).write_errors());
        }
    };

    let item = parse_macro_input!(input as Item);
    let mut name_gen = NameGen::default();

    let output = match crate::item::process_item(&item, Some(&attributes), &mut name_gen) {
        Ok(output) => output,
        Err(e) => e.write_errors().into(),
    };

    output.into()
}

#[proc_macro_error]
#[proc_macro]
pub fn impl_tuple_of_expression_handles(input: TokenStream) -> TokenStream {
    use proc_macro2::TokenStream;

    fn inner(input: TokenStream) -> Result<TokenStream, Error> {
        let n = syn::parse2::<LitInt>(input)?;
        let n = n.base10_parse::<usize>()?;

        let mut output = TokenBuffer::default();

        let mut ty_params = vec![];

        for i in 0..n {
            ty_params.push(Ident::new(&format!("X{}", i + 1), Span::call_site()));
            let tuple_ty = quote! {
                (#(embers_transpile::__private::ExpressionHandle<#ty_params>),*)
            };
            output.push(quote!{
                impl< #(#ty_params),* > embers_transpile::utils::sealed::Sealed for #tuple_ty {}
                impl< #(#ty_params),* > embers_transpile::shader_std::marker::TupleOfExpressionHandles for #tuple_ty {}
            });
        }

        Ok(output.into_token_stream())
    }

    match inner(input.into()) {
        Ok(output) => {
            println!("{output}");
            output.into()
        }
        Err(e) => e.write_errors().into(),
    }
}
