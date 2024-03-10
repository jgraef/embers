use darling::ast::NestedMeta;
use proc_macro2::{
    Literal,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use syn::{
    spanned::Spanned,
    Fields,
    Ident,
    ItemStruct,
    Type,
    Visibility,
};

use crate::{
    error::Error,
    utils::{
        ident_to_literal,
        TokenBuffer,
    },
};

pub fn process_struct(
    struct_: &ItemStruct,
    _attributes: Option<&[NestedMeta]>,
) -> Result<TokenStream, Error> {
    let vis = &struct_.vis;
    let struct_token = &struct_.struct_token;
    let ident = &struct_.ident;
    let ident_literal = ident_to_literal(&ident);
    let generics = &struct_.generics;

    let mut add_to_module = TokenBuffer::default();
    let mut struct_fields = TokenBuffer::default();
    let mut compose_field_exprs = TokenBuffer::default();
    let mut shader_type_bounds = generics.clone();
    let mut width_bounds = generics.clone();
    let mut width_const = TokenBuffer::default();
    //let mut align_to = None;
    let mut output = TokenBuffer::default();
    let mut num_fields = 0usize;

    width_const.push(quote! { 0u32 });

    let mut field_access_impl = |ident: &Ident, ty: &Type, i: u32, name: Option<Literal>| {
        let access_impl = quote! {
            type Type = ::embers_transpile::__private::Pointer<#ty, ::embers_transpile::__private::address_space::Private>;
            type Result = ::embers_transpile::__private::DeferredDereference<#ty, ::embers_transpile::__private::address_space::Private>;

            fn access(
                block_builder: &mut ::embers_transpile::__private::BlockBuilder,
                base: ::embers_transpile::__private::ExpressionHandle<Self>,
            ) -> ::embers_transpile::__private::Result<Self::Result, ::embers_transpile::__private::BuilderError> {
                ::embers_transpile::__private::access_struct_field(block_builder, base, #i)
            }
        };

        // add trait bounds for field type and Self
        let mut generics = generics.clone();
        let where_clause = generics.make_where_clause();
        where_clause.predicates.push(
            syn::parse2(quote! {
                #ty: embers_transpile::__private::ShaderType
            })
            .unwrap(),
        );
        where_clause.predicates.push(
            syn::parse2(quote! {
                Self: embers_transpile::__private::ShaderType
            })
            .unwrap(),
        );
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        output.push(quote!{
            impl #impl_generics ::embers_transpile::__private::FieldAccess<::embers_transpile::__private::UnnamedFieldAccessor<{#i}>> for #ident #ty_generics #where_clause {
                #access_impl
            }
        });

        if let Some(name) = name {
            output.push(quote!{
                impl #impl_generics ::embers_transpile::__private::FieldAccess<::embers_transpile::__private::NamedFieldAccessor<{#name}>> for #ident #ty_generics #where_clause {
                    #access_impl
                }
            });
        }
    };

    let mut add_struct_field = |name: &Ident, ty: &Type, vis: &Visibility| {
        // field in struct declaration
        struct_fields.push(quote! {
            #vis #name: ::embers_transpile::__private::ExpressionHandle<#ty>,
        });

        // expression for Compose impl
        compose_field_exprs.push(quote!{
            ::embers_transpile::__private::AsExpression::as_expression(&self.#name, &mut block_builder)?.get_naga(),
        });

        // add trait bound to field type for ShaderType impl
        let where_clause = shader_type_bounds.make_where_clause();
        where_clause.predicates.push(
            syn::parse2(quote! {
                #ty: embers_transpile::__private::ShaderType + embers_transpile::__private::Width + embers_transpile::__private::AlignTo
            })
            .unwrap(),
        );

        // add expression to const width and bounds for Width trait
        width_const.push(quote! {
            + <#ty as ::embers_transpile::__private::Width>::WIDTH
        });
        let where_clause = width_bounds.make_where_clause();
        where_clause.predicates.push(
            syn::parse2(quote! {
                #ty: ::embers_transpile::__private::Width
            })
            .unwrap(),
        );

        // impl for AlignTo
        /*if align_to.is_none() {
            let mut generics = generics.clone();
            let where_clause = compose_impl_generics.make_where_clause();
            where_clause.predicates.push(
                syn::parse2(quote! {
                    #ty: embers_transpile::__private::AlignTo
                })
                .unwrap(),
            );
            where_clause.predicates.push(
                syn::parse2(quote! {
                    Self: embers_transpile::__private::Width
                })
                .unwrap(),
            );
            let (impl_generics, ty_generics, where_clause) = compose_impl_generics.split_for_impl();
            align_to = Some(quote!{
                impl #impl_generics ::embers_transpile::__private::AlignTo for #ident #ty_generics #where_clause {
                    const ALIGN_TO: u32 = ::embers_transpile::__private::struct_alignment(
                        <#ty as ::embers_transpile::__private::AlignTo>::ALIGN_TO,
                        <Self as Width>::WIDTH,
                    );
                }
            });
        }*/

        num_fields += 1;
    };

    match &struct_.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for (i, field) in named.named.iter().enumerate() {
                let field_name = field.ident.as_ref().unwrap();
                let field_name_literal = ident_to_literal(field_name);
                let field_type = &field.ty;

                add_struct_field(field_name, field_type, &field.vis);

                add_to_module.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal)?; },
                );

                field_access_impl(ident, field_type, i as u32, Some(field_name_literal));
            }
        }
        Fields::Unnamed(unnamed) => {
            for (i, field) in unnamed.unnamed.iter().enumerate() {
                let field_type = &field.ty;

                let field_name = Ident::new(&format!("f_{i}"), field_type.span());
                add_struct_field(&field_name, field_type, &field.vis);

                add_to_module.push(quote! {
                    struct_builder.add_unnamed_field::<#field_type>()?;
                });

                field_access_impl(ident, field_type, i as u32, None);
            }
        }
    }

    output.push(quote! {
        #vis #struct_token #ident #generics {
            #struct_fields
        }
    });

    let mut compose_impl_generics = generics.clone();
    let where_clause = compose_impl_generics.make_where_clause();
    where_clause.predicates.push(
        syn::parse2(quote! {
            Self: embers_transpile::__private::ShaderType
        })
        .unwrap(),
    );
    let (impl_generics, ty_generics, where_clause) = compose_impl_generics.split_for_impl();
    output.push(quote! {
        impl #impl_generics ::embers_transpile::__private::Compose for #ident #ty_generics #where_clause {
            fn compose(
                &self,
                mut block_builder: &mut ::embers_transpile::__private::BlockBuilder
            ) -> ::embers_transpile::__private::Result<
                ::embers_transpile::__private::ExpressionHandle<Self>,
                ::embers_transpile::__private::BuilderError
            > {
                let components: [
                    ::embers_transpile::__private::Option<
                        ::embers_transpile::__private::naga::Handle<
                            ::embers_transpile::__private::naga::Expression
                        >
                    >;
                    #num_fields
                ] = [
                    #compose_field_exprs
                ];
                let components = components.into_iter().flatten().collect::<Vec<_>>();

                let compose_expr = if let Some(struct_ty) = block_builder.function_builder.module_builder.get_type_by_id_or_add_it::<Self>()?.get_data() {
                    block_builder.function_builder.add_expression::<Self>(::embers_transpile::__private::naga::Expression::Compose {
                        ty: struct_ty,
                        components,
                    })?
                }
                else {
                    ::embers_transpile::__private::ExpressionHandle::empty()
                };

                Ok(compose_expr)
            }
        }
    });

    let (impl_generics, ty_generics, where_clause) = shader_type_bounds.split_for_impl();
    output.push(quote!{
        impl #impl_generics ::embers_transpile::__private::ShaderType for #ident #ty_generics #where_clause {
            fn add_to_module(
                module_builder: &mut ::embers_transpile::__private::ModuleBuilder
            ) -> ::embers_transpile::__private::Result<
                ::embers_transpile::__private::TypeHandle,
                ::embers_transpile::__private::BuilderError
            > {
                let mut struct_builder = module_builder.add_struct(#ident_literal);
                #add_to_module
                ::embers_transpile::__private::Ok(struct_builder.build::<Self>())
            }
        }
    });

    let (impl_generics, ty_generics, where_clause) = width_bounds.split_for_impl();
    output.push(quote!{
        impl #impl_generics ::embers_transpile::__private::Width for #ident #ty_generics #where_clause {
            const WIDTH: ::embers_transpile::__private::std::primitive::u32 = #width_const;
        }
    });

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    output.push(quote!{
        impl #impl_generics ::embers_transpile::__private::AlignTo for #ident #ty_generics #where_clause {
            const ALIGN_TO: ::embers_transpile::__private::std::primitive::u32 = 4; // fixme
        }
        impl #impl_generics ::embers_transpile::__private::Name for #ident #ty_generics #where_clause {
            const NAME: &'static ::embers_transpile::__private::std::primitive::str = #ident_literal;
        }
    });

    Ok(output.to_token_stream())
}
