use darling::{
    ast::NestedMeta,
    FromAttributes,
    FromMeta,
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{
        Parse,
        ParseStream,
    },
    parse_macro_input,
    Attribute,
    Expr,
    Ident,
    Local,
    Meta,
    Token,
    Type,
    Visibility,
};

use crate::{
    error::Error,
    utils::ident_to_literal,
};

#[derive(Debug, FromMeta)]
pub(crate) enum StorageAccess {
    #[darling(word)]
    Read,
    Write,
}

#[derive(Debug, FromMeta)]
pub(crate) enum AddressSpace {
    Function,
    Private,
    WorkGroup,
    Uniform,
    Storage(StorageAccess),
    Handle,
    PushConstant,
}

impl Default for AddressSpace {
    fn default() -> Self {
        Self::Private
    }
}

#[derive(Debug, Default, FromAttributes)]
#[darling(attributes(embers))]
pub struct GlobalAttributes {
    pub group: Option<u32>,
    pub binding: Option<u32>,
    #[darling(default)]
    pub address_space: AddressSpace,
    //pub attrs: Vec<Attribute>,
}

pub(crate) struct GlobalVar {
    pub attrs: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: Ident,
    pub ty: Type,
    pub init: Option<Expr>,
}

impl Parse for GlobalVar {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let attrs = input.call(Attribute::parse_outer)?;

        let visibility: Visibility = input.parse()?;
        input.parse::<Token![static]>()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;

        let lookahead = input.lookahead1();
        let init = if lookahead.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            let init: Expr = input.parse()?;
            Some(init)
        }
        else {
            None
        };

        input.parse::<Token![;]>()?;

        Ok(Self {
            attrs,
            visibility,
            name,
            ty,
            init,
        })
    }
}

impl GlobalVar {
    pub(crate) fn process(self) -> Result<TokenStream, Error> {
        let attributes = GlobalAttributes::from_attributes(&self.attrs)?;
        let Self {
            visibility,
            name,
            ty,
            ..
        } = self;
        let name_lit = ident_to_literal(&name);

        let address_space_expr = match attributes.address_space {
            AddressSpace::Function => {
                quote! { ::embers_transpile::__private::address_space::Function }
            }
            AddressSpace::Private => {
                quote! { ::embers_transpile::__private::address_space::Private }
            }
            AddressSpace::WorkGroup => {
                quote! { ::embers_transpile::__private::address_space::WorkGroup }
            }
            AddressSpace::Uniform => {
                quote! { ::embers_transpile::__private::address_space::Uniform }
            }
            AddressSpace::Storage(access) => {
                match access {
                    StorageAccess::Read => {
                        quote! { ::embers_transpile::__private::address_space::StorageRead }
                    }
                    StorageAccess::Write => {
                        quote! { ::embers_transpile::__private::address_space::StorageReadWrite }
                    }
                }
            }
            AddressSpace::Handle => quote! { ::embers_transpile::__private::AddressSpace::Handle },
            AddressSpace::PushConstant => {
                quote! { ::embers_transpile::__private::AddressSpace::PushConstant }
            }
        };

        let binding_expr = if let Some(binding) = attributes.binding {
            let group = attributes.group.unwrap_or_default();
            quote! {
                ::embers_transpile::__private::Some(::embers_transpile::__private::naga::ResourceBinding {
                    group: #group,
                    binding: #binding,
                })
            }
        }
        else {
            quote! { ::embers_transpile::__private::None }
        };

        let output = quote! {
            #[allow(non_camel_case_types)]
            #visibility struct #name;

            impl ::embers_transpile::__private::GlobalVariable for #name {
                type Type = #ty;
                type AddressSpace = #address_space_expr;
                const NAME: &'static str = #name_lit;
                const BINDING: ::embers_transpile::__private::Option<::embers_transpile::__private::naga::ResourceBinding> = #binding_expr;
            }
        };

        Ok(output)
    }
}
