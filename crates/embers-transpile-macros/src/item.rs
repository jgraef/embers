use darling::ast::NestedMeta;
use proc_macro2::TokenStream;
use syn::Item;
use quote::quote;

use crate::error::Error;

pub fn process_item(item: &Item, attributes: Option<&[NestedMeta]>) -> Result<TokenStream, Error> {
    let output = match item {
        Item::Fn(func) => crate::function::process_bare_function(&func, attributes)?,
        Item::Trait(trait_) => crate::r#trait::process_trait(&trait_, attributes)?,
        Item::Impl(impl_) => crate::r#trait::process_impl(&impl_, attributes)?,
        Item::Struct(struct_) => crate::r#struct::process_struct(&struct_, attributes)?,
        Item::Mod(mod_) => crate::module::process_module(&mod_, attributes)?,
        //_ => panic!("invalid use of #[transpile] macro"),
        item => quote! { #item },
    };

    Ok(output)
}
