use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    DataStruct,
    Fields,
    Ident,
};

use crate::{
    args::StructArgs,
    error::Error,
    utils::ident_to_literal,
};

pub fn impl_shader_type_for_struct(
    ident: &Ident,
    strct: &DataStruct,
    args: &StructArgs,
) -> Result<TokenStream, Error> {
    // todo: how do we make the type name unique????
    // todo: handle generics

    let ident_literal = ident_to_literal(&ident);

    let mut struct_fields = vec![];
    let mut accessor_impls = vec![];

    match &strct.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for (i, field) in named.named.iter().enumerate() {
                let field_name_literal = ident_to_literal(field.ident.as_ref().unwrap());
                let field_type = &field.ty;
                struct_fields.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal)?; },
                );
                let field_access_impl = quote! {
                    const INDEX: usize = #i;
                    type Type = #field_type;
                };
                accessor_impls.push(
                    quote! {
                        impl ::embers_transpile::__private::FieldAccess<::embers_transpile::__private::UnnamedFieldAccessor<{#i}>> for #ident {
                            #field_access_impl
                        }
                        impl ::embers_transpile::__private::FieldAccess<::embers_transpile::__private::NamedFieldAccessor({#field_name_literal})> for #ident {
                            #field_access_impl
                        }
                    }
                )
            }
        }
        Fields::Unnamed(unnamed) => {
            for (i, field) in unnamed.unnamed.iter().enumerate() {
                let field_type = &field.ty;
                struct_fields.push(quote! { struct_builder.add_unnamed_field::<#field_type>()?; });
                accessor_impls.push(
                    quote! {
                        impl ::embers_transpile::__private::FieldAccess<::embers_transpile::__private::UnnamedFieldAccessor<{#i}>> for #ident {
                            const INDEX: usize = #i;
                            type Type = #field_type;
                        }
                    }
                );
            }
        }
    }

    let generated = quote! {
        impl ::embers_transpile::__private::ShaderType for #ident {
            fn add_to_module(module_builder: &mut ::embers_transpile::__private::ModuleBuilder) -> ::embers_transpile::__private::Result<::embers_transpile::__private::TypeHandle, ::embers_transpile::__private::BuilderError> {
                let mut struct_builder = module_builder.add_struct(#ident_literal);
                #(#struct_fields)*
                ::embers_transpile::__private::Ok(struct_builder.build::<Self>())
            }
        }
        #(#accessor_impls)*
    };

    Ok(generated)
}
