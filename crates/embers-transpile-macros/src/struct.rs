use darling::ast::NestedMeta;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    token::Token,
    DataStruct,
    Fields,
    Generics,
    Ident,
    ItemStruct,
    Type,
    TypeGenerics,
    WhereClause,
};

use crate::{
    error::Error,
    utils::ident_to_literal,
};

pub fn impl_shader_type_for_struct(
    ident: &Ident,
    strct: &DataStruct,
    _attributes: Option<&[NestedMeta]>,
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

pub fn process_struct(
    struct_: &ItemStruct,
    _attributes: Option<&[NestedMeta]>,
) -> Result<TokenStream, Error> {
    let vis = &struct_.vis;
    let struct_token = &struct_.struct_token;
    let ident = &struct_.ident;
    let ident_literal = ident_to_literal(&ident);
    let generics = &struct_.generics;

    let mut struct_fields = vec![];
    let mut accessor_impls = vec![];

    fn field_access_impl(ty: &Type, i: usize) -> TokenStream {
        quote! {
            type Type = ::embers_transpile::__private::Pointer<#ty, ::embers_transpile::__private::address_space::Private>;
            type Result = ::embers_transpile::__private::DeferredDereference<#ty, ::embers_transpile::__private::address_space::Private>;

            fn access(
                function_builder: &mut ::embers_transpile::__private::FunctionBuilder,
                base: ::embers_transpile::__private::ExpressionHandle<Self>,
            ) -> ::embers_transpile::__private::Result<Self::Result, ::embers_transpile::__private::BuilderError> {
                ::embers_transpile::__private::access_struct_field(function_builder, base, #i)
            }
        }
    }

    match &struct_.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for (i, field) in named.named.iter().enumerate() {
                let field_name_literal = ident_to_literal(field.ident.as_ref().unwrap());
                let field_type = &field.ty;
                struct_fields.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal)?; },
                );
                let field_access_impl = field_access_impl(field_type, i);
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
                let field_access_impl = field_access_impl(field_type, i);
                accessor_impls.push(
                    quote! {
                        impl ::embers_transpile::__private::FieldAccess<::embers_transpile::__private::UnnamedFieldAccessor<{#i}>> for #ident {
                            #field_access_impl
                        }
                    }
                );
            }
        }
    }

    let generated = quote! {
        #vis #struct_token #ident #generics {
            handle: ::embers_transpile::__private::ExpressionHandle<Self>,
        }

        impl ::embers_transpile::__private::AsExpression<Self> for #ident {
            fn as_expression(
                &self,
                _function_builder: &mut ::embers_transpile::__private::FunctionBuilder
            ) -> ::embers_transpile::__private::Result<
                ::embers_transpile::__private::ExpressionHandle<Self>,
                ::embers_transpile::__private::BuilderError
            > {
                ::embers_transpile::__private::Ok(self.handle.clone())
            }
        }

        impl ::embers_transpile::__private::FromExpression<Self> for #ident {
            fn from_expression(
                handle: ::embers_transpile::__private::ExpressionHandle<Self>,
            ) -> ::embers_transpile::__private::Result<
                Self,
                ::embers_transpile::__private::BuilderError
            > {
                ::embers_transpile::__private::Ok(Self { handle })
            }
        }

        impl ::embers_transpile::__private::ShaderType for #ident {
            fn add_to_module(
                module_builder: &mut ::embers_transpile::__private::ModuleBuilder
            ) -> ::embers_transpile::__private::Result<
                ::embers_transpile::__private::TypeHandle,
                ::embers_transpile::__private::BuilderError
            > {
                let mut struct_builder = module_builder.add_struct(#ident_literal);
                #(#struct_fields)*
                ::embers_transpile::__private::Ok(struct_builder.build::<Self>())
            }
        }

        #(#accessor_impls)*
    };

    Ok(generated)
}
