use darling::ast::NestedMeta;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Item;

use crate::{
    error::Error,
    utils::NameGen,
};

pub fn process_item(
    item: &Item,
    attributes: Option<&[NestedMeta]>,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let output = match item {
        Item::Fn(func) => crate::function::process_bare_function(&func, attributes, name_gen)?,
        Item::Trait(trait_) => crate::r#trait::process_trait(&trait_, attributes, name_gen)?,
        Item::Impl(impl_) => crate::r#trait::process_impl(&impl_, attributes, name_gen)?,
        Item::Struct(struct_) => crate::r#struct::process_struct(&struct_, attributes)?,
        Item::Mod(mod_) => crate::module::process_module(&mod_, attributes, name_gen)?,
        //_ => panic!("invalid use of #[transpile] macro"),
        item => quote! { #item },
    };

    Ok(output)
}
