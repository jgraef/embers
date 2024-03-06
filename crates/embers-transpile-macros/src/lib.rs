#![allow(dead_code)]

mod closure;
mod error;
mod function;
mod item;
mod module;
mod r#struct;
mod r#trait;
mod utils;

use closure::Closure;
use darling::ast::NestedMeta;
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::{
    parse_macro_input,
    Item,
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

    let output = match crate::item::process_item(&item, Some(&attributes)) {
        Ok(output) => output,
        Err(e) => e.write_errors().into(),
    };

    output.into()
}

#[proc_macro_error]
#[proc_macro]
pub fn closure(input: TokenStream) -> TokenStream {
    let closure = parse_macro_input!(input as Closure);
    let output = closure.process().unwrap();
    output.into()
}
