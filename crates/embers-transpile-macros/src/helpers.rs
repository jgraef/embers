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

pub fn internal_impl_arguments(input: TokenStream) -> Result<TokenStream, Error> {
    let n = syn::parse2::<LitInt>(input)?;
    let n = n.base10_parse::<usize>()?;

    let mut output = TokenBuffer::default();

    let mut ty_params = TokenBuffer::default();
    let mut match_arms = TokenBuffer::default();
    let mut expression_handles = TokenBuffer::default();
    let mut arguments = TokenBuffer::default();
    let mut arguments_new = TokenBuffer::default();

    for i in 0..n + 1 {
        output.push(quote! {
            impl<#ty_params> sealed1::Sealed for (#ty_params) {}
            impl<#ty_params> sealed2::Sealed for (#expression_handles) {}
            impl<#ty_params> sealed3::Sealed for (#arguments) {}

            impl<#ty_params> Arguments for (#ty_params) {
                type AsExpressionHandles = (#expression_handles);
                type AsArguments = (#arguments);
            }

            impl<#ty_params> TupleOfExpressionHandles for (#expression_handles) {}

            impl<#ty_params> TupleOfArguments for (#arguments) {
                fn new() -> Self {
                    (#arguments_new)
                }
            }
        });

        let ty = Ident::new(&format!("X{}", i + 1), Span::call_site());
        expression_handles.push(quote! {
            ExpressionHandle<#ty>,
        });
        arguments.push(quote! {
            Argument<#ty>,
        });
        arguments_new.push(quote! {
            Argument::default(),
        });
        ty_params.push(quote! { #ty, });
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
        if let Some(ty) = arg_type_params.first() {
            self_type_param =
                Some(quote! { S: ::embers_transpile::__private::MaybeSelf<Inner=#ty> + 'static, });
            first_arg_type = Some(quote! { S, }); // trailing comma very
                                                  // important!
        }

        let defaults = arg_type_params
            .iter()
            .map(|_| quote! { ::embers_transpile::__private::std::default::Default::default() });

        output.push(quote!{
            impl<
                //#first_arg_type
                #(#arg_type_params,)*
                FnItem,
            > ::embers_transpile::__private::AsExpression<FnItem::FunctionType>
            //for fn(#first_arg_type #(::embers_transpile::__private::Argument<#arg_type_params_tail>),*) -> FnItem
            for fn(#(::embers_transpile::__private::Argument<#arg_type_params>),*) -> FnItem
            where
                //#self_type_param
                //#(#arg_type_params: 'static,)*
                FnItem: ::embers_transpile::__private::FnItem,
                //FnItem::FunctionType: ::embers_transpile::__private::FunctionTrait<
                //    Args = (#(::embers_transpile::__private::ExpressionHandle<#arg_type_params>,)*),
                //    Output = FnItem::Output,
                //>,
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
