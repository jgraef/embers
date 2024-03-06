#![allow(dead_code)]

mod args;
mod error;
mod function;
mod global;
mod item;
mod module;
mod r#struct;
mod r#trait;
mod utils;

use darling::ast::NestedMeta;
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::{
    parse_macro_input,
    Item,
};

use crate::global::GlobalVar;

/*
#[derive(FromDeriveInput)]
#[darling(attributes(ricsl), forward_attrs(allow, doc, cfg))]
struct DeriveOpts {
    ident: syn::Ident,
    attrs: Vec<syn::Attribute>,
    #[darling(default)]
    args: StructArgs,
}

#[proc_macro_derive(ShaderType)]
pub fn derive_shader_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let opts = match DeriveOpts::from_derive_input(&input) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(darling::Error::from(e).write_errors());
        }
    };

    let output = match &input.data {
        Data::Struct(s) => impl_shader_type_for_struct(&opts.ident, s, &opts.args).unwrap(),
        Data::Enum(_) => panic!("enum not supported"),
        Data::Union(_) => panic!("union not supported"),
    };

    output.into()
}
*/

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
pub fn global(input: TokenStream) -> TokenStream {
    //let input: proc_macro2::TokenStream = input.into();

    let global = parse_macro_input!(input as GlobalVar);
    let output = global.process().unwrap();

    //println!("{output}");

    output.into()
}
