#![allow(dead_code)]

mod closure;
mod error;
mod expression;
mod function;
mod helpers;
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
    match helpers::impl_tuple_of_expression_handles(input.into()) {
        Ok(output) => output.into(),
        Err(e) => e.write_errors().into(),
    }
}

#[proc_macro_error]
#[proc_macro]
pub fn impl_functions(input: TokenStream) -> TokenStream {
    match helpers::impl_functions(input.into()) {
        Ok(output) => output.into(),
        Err(e) => e.write_errors().into(),
    }
}
