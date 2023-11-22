use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    DataStruct,
    Fields,
    Ident,
};

use crate::{
    error::Error,
    utils::ident_to_literal,
};

pub fn impl_ricsl_type_for_struct(ident: &Ident, strct: &DataStruct) -> Result<TokenStream, Error> {
    // todo: how do we make the type name unique????
    // todo: handle generics

    let ident_literal = ident_to_literal(&ident);

    let mut struct_fields = vec![];
    let mut phantom_eval = vec![];

    match &strct.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for field in &named.named {
                let field_name_literal = ident_to_literal(field.ident.as_ref().unwrap());
                let field_type = &field.ty;
                struct_fields.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal); },
                );
                phantom_eval.push(
                    quote! { && <#field_type as ::embers_ricsl::__private::RicslType>::PHANTOM },
                );
            }
        }
        Fields::Unnamed(unnamed) => {
            for field in &unnamed.unnamed {
                let field_type = &field.ty;
                struct_fields.push(quote! { struct_builder.add_unnamed_field::<#field_type>(); });
            }
        }
    }

    let generated = quote! {
        impl ::embers_ricsl::__private::RicslType for #ident {
            const PHANTOM: bool = {
                true #(#phantom_eval)*
            };

            fn add_to_module(module_builder: &mut ::embers_ricsl::__private::ModuleBuilder) -> ::embers_ricsl::__private::Handle<::embers_ricsl::__private::Type> {
                let mut struct_builder = module_builder.add_struct::<Self>(#ident_literal);
                #(#struct_fields)*
                struct_builder.build()
            }
        }
    };

    Ok(generated)
}
