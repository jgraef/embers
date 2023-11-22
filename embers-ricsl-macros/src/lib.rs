#![allow(dead_code, unused_variables)]

mod error;
mod functions;
mod structs;
mod traits;
mod utils;
mod args;

use darling::{
    ast::NestedMeta, FromMeta,
};
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::{
    parse_macro_input,
    Data,
    DeriveInput,
    Item,
};

use crate::{
    functions::{
        process_entrypoint,
        process_function,
    },
    structs::impl_ricsl_type_for_struct,
    traits::process_trait,
    args::{FnArgs, TraitArgs},
};

#[proc_macro_derive(RicslType)]
pub fn derive_ricsl_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let output = match &input.data {
        Data::Struct(s) => impl_ricsl_type_for_struct(&input.ident, s).unwrap(),
        Data::Enum(_) => panic!("enum not supported"),
        Data::Union(_) => panic!("union not supported"),
    };

    output.into()
}


#[proc_macro_error]
#[proc_macro_attribute]
pub fn ricsl(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let attrs = match NestedMeta::parse_meta_list(attrs.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(darling::Error::from(e).write_errors());
        }
    };

    let item: Item = syn::parse(input).unwrap();
    let output = match item {
        Item::Fn(func) => {
            let args = match FnArgs::from_list(&attrs) {
                Ok(v) => v,
                Err(e) => {
                    return TokenStream::from(e.write_errors());
                }
            };

            if args.entrypoint {
                process_entrypoint(&func, &args).unwrap()
            }
            else {
                process_function(&func, &args).unwrap()
            }
        }
        Item::Trait(trait_) => {
            let args = match TraitArgs::from_list(&attrs) {
                Ok(v) => v,
                Err(e) => {
                    return TokenStream::from(e.write_errors());
                }
            };

            process_trait(&trait_, &args).unwrap()
        },
        _ => panic!("invalid use of #[ricsl] macro"),
    };

    println!("{output}");

    output.into()
}
