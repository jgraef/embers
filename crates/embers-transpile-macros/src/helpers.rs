use proc_macro2::{
    Span,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use syn::{
    Ident,
    Index,
    LitInt,
};

use crate::{
    error::Error,
    utils::TokenBuffer,
};

pub fn impl_tuple_of_expression_handles(input: TokenStream) -> Result<TokenStream, Error> {
    let n = syn::parse2::<LitInt>(input)?;
    let n = n.base10_parse::<usize>()?;

    let mut output = TokenBuffer::default();

    let mut ty_params = vec![];
    let mut match_arms = TokenBuffer::default();

    for i in 0..n + 1 {
        let tuple_ty = quote! {
            (#(::embers_transpile::__private::ExpressionHandle<#ty_params>,)*)
        };

        output.push(quote!{
            impl< #(#ty_params),* > Sealed for #tuple_ty {}
            impl< #(#ty_params),* > ::embers_transpile::shader_std::marker::TupleOfExpressionHandles for #tuple_ty {
                fn project_as_dyn(&self, index: usize) -> ::embers_transpile::__private::DynExpressionHandle {
                    match index {
                        #match_arms
                        _ => panic!("invalid index {index} for tuple"),
                    }
                }
            }
        });

        ty_params.push(Ident::new(&format!("X{}", i + 1), Span::call_site()));
        let tuple_index = Index::from(i);
        match_arms.push(quote! {
            #i => self.#tuple_index.as_dyn(),
        })
    }

    Ok(output.into_token_stream())
}

pub fn impl_functions(input: TokenStream) -> Result<TokenStream, Error> {
    fn impl_for(arg_type_params: &[Ident], output: &mut TokenBuffer) {
        let arg_type_params_tail = arg_type_params.get(1..).unwrap_or_default();

        let mut self_type_param = None;
        let mut first_arg_type = None;
        let mut self_kind = None;
        if let Some(ty) = arg_type_params.first() {
            self_type_param =
                Some(quote! { S: ::embers_transpile::__private::MaybeSelf<Inner=#ty> + 'static, });
            first_arg_type = Some(quote! { S, }); // trailing comma very important!
            self_kind = Some(quote! { S });
        }

        let defaults = arg_type_params
            .iter()
            .map(|_| quote! { ::embers_transpile::__private::std::default::Default::default() });
        let arg_indices = arg_type_params
            .iter()
            .enumerate()
            .map(|(i, _)| Index::from(i));

        output.push(quote!{
            impl<
                #self_type_param
                #(#arg_type_params: 'static,)*
                FnItem: ::embers_transpile::__private::FnItem,
                //F: Fn(#first_arg_type #(::embers_transpile::__private::Argument<#arg_type_params_tail>),*) -> FnItem,
            > ::embers_transpile::__private::AsExpression<FnItem::FunctionType> //for F
            for fn(#first_arg_type #(::embers_transpile::__private::Argument<#arg_type_params_tail>),*) -> FnItem
            where
                FnItem::FunctionType: ::embers_transpile::__private::FunctionTrait<
                    Args = (#(::embers_transpile::__private::ExpressionHandle<#arg_type_params>,)*)
                >,
            {
                fn as_expression(
                    &self,
                    block_builder: &mut embers_transpile::__private::BlockBuilder,
                ) -> ::embers_transpile::__private::Result<
                    ::embers_transpile::__private::ExpressionHandle<FnItem::FunctionType>,
                    ::embers_transpile::__private::BuilderError
                > {
                    let fn_item = self(
                        #(#defaults),*
                    );
                    fn_item.get_function_expression(&mut block_builder.function_builder.module_builder)
                }
            }
        });
    }

    let n = syn::parse2::<LitInt>(input)?;
    let n = n.base10_parse::<usize>()?;

    let mut output = TokenBuffer::default();

    let mut arg_type_params = vec![];

    for i in 0..n + 1 {
        impl_for(&arg_type_params, &mut output);
        arg_type_params.push(Ident::new(&format!("A{}", i + 1), Span::call_site()));
    }

    Ok(output.into_token_stream())
}

/*
impl<
                #self_type_param
                #(#arg_type_params: 'static,)*
                R: 'static,
                F: Fn(#first_arg_type #(::embers_transpile::__private::Argument<#arg_type_params_tail>),*) -> impl ::embers_transpile::__private::FnItem<Output = R>,
            > ::embers_transpile::__private::AsExpression<F> for F
            {
                fn as_expression(
                    &self,
                    block_builder: &mut embers_transpile::__private::BlockBuilder,
                ) -> ::embers_transpile::__private::Result<
                    ::embers_transpile::__private::ExpressionHandle<F>,
                    ::embers_transpile::__private::BuilderError
                > {
                    let fn_item = self(
                        #(#defaults),*
                    );
                    fn_item.get_function_expression(&mut block_builder.function_builder.module_builder)
                }
            }
*/
