use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{Ident, Index, LitInt};

use crate::{error::Error, utils::TokenBuffer};



pub fn impl_tuple_of_expression_handles(input: TokenStream) -> Result<TokenStream, Error> {
    let n = syn::parse2::<LitInt>(input)?;
    let n = n.base10_parse::<usize>()?;

    let mut output = TokenBuffer::default();

    let mut ty_params = vec![];

    for i in 0..n + 1 {
        let tuple_ty = quote! {
            (#(embers_transpile::__private::ExpressionHandle<#ty_params>),*)
        };
        output.push(quote!{
            impl< #(#ty_params),* > embers_transpile::utils::sealed::Sealed for #tuple_ty {}
            impl< #(#ty_params),* > embers_transpile::shader_std::marker::TupleOfExpressionHandles for #tuple_ty {}
        });
        ty_params.push(Ident::new(&format!("X{}", i + 1), Span::call_site()));
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
            self_type_param = Some(quote!{ S: ::embers_transpile::__private::MaybeSelf<Inner=#ty> + 'static, });
            first_arg_type = Some(quote!{ S, }); // trailing comma very important!
            self_kind = Some(quote! { S });
        }

        let defaults = arg_type_params.iter().map(|_| quote!{ ::embers_transpile::__private::std::default::Default::default() });
        let arg_indices = arg_type_params.iter().enumerate().map(|(i, _)| Index::from(i));

        output.push(quote!{
            impl<
                #self_type_param
                #(#arg_type_params: 'static,)*
                R: 'static,
                F: Fn(#first_arg_type #(::embers_transpile::__private::Argument<#arg_type_params_tail>),*) -> ::embers_transpile::__private::Return<R>,
                G: ::embers_transpile::__private::GenerateFunction,
            > ::embers_transpile::__private::AsExpression<::embers_transpile::__private::FunctionType<(#(#arg_type_params,)*), R, G, #self_kind>> for F
            {
                fn as_expression(
                    &self,
                    block_builder: &mut embers_transpile::__private::BlockBuilder,
                ) -> ::embers_transpile::__private::Result<
                    ::embers_transpile::__private::ExpressionHandle<
                        ::embers_transpile::__private::FunctionType<(#(#arg_type_params,)*), R, G, #self_kind>
                    >,
                    ::embers_transpile::__private::BuilderError
                > {
                    let ::embers_transpile::__private::Return {
                        generator, func_id, ..
                    } = self(
                        #(#defaults),*
                    );
                    block_builder
                        .function_builder
                        .module_builder
                        .add_function(func_id, generator)?;
                    ::embers_transpile::__private::Ok(::embers_transpile::__private::ExpressionHandle::empty())
                }
            }
            
            impl<
                #self_type_param
                #(#arg_type_params: 'static,)*
                R: 'static,
                G: ::embers_transpile::__private::GenerateFunction,
            > ::embers_transpile::__private::FunctionTrait<(#(::embers_transpile::__private::ExpressionHandle<#arg_type_params>,)*)>
            for ::embers_transpile::__private::FunctionType<(#(#arg_type_params,)*), R, G, #self_kind>
            {
                type Output = R;

                fn call(
                    func: ::embers_transpile::__private::ExpressionHandle<Self>,
                    args: (#(::embers_transpile::__private::ExpressionHandle<#arg_type_params>,)*),
                    block_builder: &mut ::embers_transpile::__private::BlockBuilder,
                ) -> ::embers_transpile::__private::Result<
                    ::embers_transpile::__private::ExpressionHandle<Self::Output>,
                    ::embers_transpile::__private::BuilderError
                > {
                    let naga_func = block_builder
                        .function_builder
                        .module_builder
                        .get_type::<Self>()?
                        .try_get_code()?;
                    block_builder.add_call::<R>(
                        naga_func,
                        [
                            #(args.#arg_indices.as_dyn(),)*
                            // the function data is passed as last argument, as expected by closures.
                            // for normal functions this will be None, since Self doesn't have a naga type.
                            func.as_dyn()
                        ],
                    )
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
