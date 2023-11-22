use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    DataStruct,
    Fields,
    Ident,
};

use crate::{
    args::{
        StdFlag,
        StructArgs,
    },
    error::Error,
    utils::ident_to_literal,
};

pub fn impl_ricsl_type_for_struct(
    ident: &Ident,
    strct: &DataStruct,
    args: &StructArgs,
) -> Result<TokenStream, Error> {
    // todo: how do we make the type name unique????
    // todo: handle generics

    let private = args.private();

    let ident_literal = ident_to_literal(&ident);

    let mut struct_fields = vec![];

    match &strct.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for field in &named.named {
                let field_name_literal = ident_to_literal(field.ident.as_ref().unwrap());
                let field_type = &field.ty;
                struct_fields.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal); },
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
        impl #private::RicslType for #ident {
            fn add_to_module(module_builder: &mut ::embers_ricsl::__private::ModuleBuilder) -> #private::TypeHandle {
                let mut struct_builder = module_builder.add_struct::<Self>(#ident_literal);
                #(#struct_fields)*
                struct_builder.build()
            }
        }
    };

    Ok(generated)
}
