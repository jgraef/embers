#![allow(dead_code)]

mod args;
mod error;
mod functions;
mod global;
mod structs;
mod traits;
mod utils;

use darling::{
    ast::NestedMeta,
    FromDeriveInput,
    FromMeta,
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
    args::{
        FnArgs,
        ImplArgs,
        StructArgs,
        TraitArgs,
    },
    functions::{
        process_bare_function,
        process_entrypoint,
    },
    global::GlobalVar,
    structs::impl_shader_type_for_struct,
    traits::{
        process_impl,
        process_trait,
    },
};

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

#[proc_macro_error]
#[proc_macro_attribute]
pub fn transpile(attrs: TokenStream, input: TokenStream) -> TokenStream {
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
                process_bare_function(&func, &args).unwrap()
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
        }
        Item::Impl(impl_) => {
            let args = match ImplArgs::from_list(&attrs) {
                Ok(v) => v,
                Err(e) => {
                    return TokenStream::from(e.write_errors());
                }
            };

            process_impl(&impl_, &args).unwrap()
        }
        _ => panic!("invalid use of #[ricsl] macro"),
    };

    println!("{output}");

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
