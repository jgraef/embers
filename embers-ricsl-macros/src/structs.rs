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
    let mut accessor_impls = vec![];
    let mut compose_impl = None;

    match &strct.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for (i, field) in named.named.iter().enumerate() {
                let field_name_literal = ident_to_literal(field.ident.as_ref().unwrap());
                let field_type = &field.ty;
                struct_fields.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal); },
                );
                let field_access_impl = quote! {
                    const INDEX: usize = #i;
                    type Type = #field_type;
                };
                accessor_impls.push(
                    quote! {
                        impl #private::FieldAccess<{ #private::FieldAccessor::Unnamed(#i) }> for #ident {
                            #field_access_impl
                        }
                        impl #private::FieldAccess<{ #private::FieldAccessor::Named(#field_name_literal) }> for #ident {
                            #field_access_impl
                        }
                    }
                )
            }
        }
        Fields::Unnamed(unnamed) => {
            let mut compose_first = None;
            let mut compose_tail = vec![];

            for (i, field) in unnamed.unnamed.iter().enumerate() {
                let field_type = &field.ty;
                struct_fields.push(quote! { struct_builder.add_unnamed_field::<#field_type>(); });
                accessor_impls.push(
                    quote! {
                        impl #private::FieldAccess<{ #private::FieldAccessor::Unnamed(#i) }> for #ident {
                            const INDEX: usize = #i;
                            type Type = #field_type;
                        }
                    }
                );
                if i == 0 {
                    compose_first = Some(quote! { #private::ExpressionHandle<#field_type> });
                }
                else {
                    compose_tail.push(quote! { #private::ExpressionHandle<#field_type> });
                }
            }

            let compose_first = compose_first.unwrap_or_else(|| quote!{ () });
            let compose_tail = quote!{ (#(#compose_tail),*) };

            compose_impl = Some(quote! {
                impl #private::Callable<#compose_first, #compose_tail, #private::ExpressionHandle<Self>> for #ident {

                }
            });
        }
    }

    let generated = quote! {
        impl #private::RicslType for #ident {
            fn add_to_module(module_builder: &mut #private::ModuleBuilder) -> #private::TypeHandle {
                let mut struct_builder = module_builder.add_struct::<Self>(#ident_literal);
                #(#struct_fields)*
                struct_builder.build()
            }
        }
        #(#accessor_impls)*
    };

    Ok(generated)
}
