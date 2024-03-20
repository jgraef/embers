use std::ops::Deref;

use proc_macro2::TokenStream;
use quote::{
    quote,
    ToTokens,
};
use syn::{
    spanned::Spanned, BinOp, Expr, Ident, Member, PatPath, Path, UnOp, ExprPath,
};

use crate::{
    error::Error,
    utils::{
        ident_to_literal,
        NameGen,
        TokenBuffer,
    },
};

#[must_use]
pub enum ExprOut {
    Ident(Ident),
    Path(ExprPath),
}

impl From<ExprPath> for ExprOut {
    fn from(path: ExprPath) -> Self {
        Self::Path(path)
    }
}

impl From<Ident> for ExprOut {
    fn from(ident: Ident) -> Self {
        Self::Ident(ident)
    }
}

impl ToTokens for ExprOut {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ExprOut::Ident(ident) => {
                tokens.extend(quote! { #ident });
            }
            ExprOut::Path(path) => {
                tokens.extend(quote! { #path });
            }
        }
    }
}

pub fn process_expr(
    input: &Expr,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    fn emit_func_call<'a>(func: impl ToTokens, args: impl IntoIterator<Item = &'a Expr>, output: &mut TokenBuffer, name_gen: &mut NameGen) -> Result<ExprOut, Error> {
        let func_var = name_gen.tmp_var("func");
        output.push(quote!{
            let #func_var = embers_transpile::__private::AsExpression::as_expression(&#func, &mut _block_builder)?;
        });

        let mut arg_vars = vec![];
        for arg in args {
            let arg = process_expr(arg, output, name_gen)?;
            let arg_var = name_gen.tmp_var("arg");
            output.push(quote!{
                let #arg_var = embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _block_builder)?;
            });
            arg_vars.push(arg_var);
        }

        let result_var = name_gen.tmp_var("result");
        output.push(quote! {
            let #result_var = #func_var.call((#(#arg_vars,)*), &mut _block_builder)?;
        });

        Ok(result_var.into())
    }

    macro_rules! emit_func_call_std {
        ($func_path:path, $($args:expr),*) => {{
            let _args = [$($args),*].into_iter().map(Deref::deref);
            emit_func_call(quote!{ ::embers_transpile::__private::shader_std::$func_path }, _args, output, name_gen)
        }};
    }

    let expr_out = match input {
        Expr::Assign(_assign) => todo!("process_expr -> Expr::Assign"),
        Expr::Binary(binary) => {
            let left = &binary.left;
            let right = &binary.right;

            let out_result = match &binary.op {
                BinOp::Add(_) => emit_func_call_std!(ops::Add::add, left, right),
                BinOp::Sub(_) => emit_func_call_std!(ops::Sub::sub, left, right),
                BinOp::Mul(_) => emit_func_call_std!(ops::Mul::mul, left, right),
                BinOp::Div(_) => emit_func_call_std!(ops::Div::div, left, right),
                BinOp::Rem(_) => emit_func_call_std!(ops::Rem::rem, left, right),
                BinOp::And(_) => emit_func_call_std!(ops::And::and, left, right),
                BinOp::Or(_) => emit_func_call_std!(ops::Or::or, left, right),
                BinOp::BitXor(_) => emit_func_call_std!(ops::BitXor::bit_xor, left, right),
                BinOp::BitAnd(_) => emit_func_call_std!(ops::BitAnd::bit_and, left, right),
                BinOp::BitOr(_) => emit_func_call_std!(ops::BitOr::bit_or, left, right),
                BinOp::Shl(_) => emit_func_call_std!(ops::Shl::shl, left, right),
                BinOp::Shr(_) => emit_func_call_std!(ops::Shr::shr, left, right),
                //BinOp::Eq(_) => emit_func_call_std!(ops::Eq::eq, left, right),
                //BinOp::Lt(_) => emit_func_call_std!(ops::Lt::lt, left, right),
                //BinOp::Le(_) => emit_func_call_std!(ops::Deref::deref, left, right),
                //BinOp::Ne(_) => emit_func_call_std!(ops::Deref::deref, left, right),
                //BinOp::Ge(_) => emit_func_call_std!(ops::Deref::deref, left, right),
                //BinOp::Gt(_) => emit_func_call_std!(ops::Deref::deref, left, right),
                BinOp::AddAssign(_) => emit_func_call_std!(ops::AddAssign::add_assign, left, right),
                BinOp::SubAssign(_) => emit_func_call_std!(ops::SubAssign::sub_assign, left, right),
                BinOp::MulAssign(_) => emit_func_call_std!(ops::MulAssign::mul_asign, left, right),
                BinOp::DivAssign(_) => emit_func_call_std!(ops::DivAssign::div_assign, left, right),
                BinOp::RemAssign(_) => emit_func_call_std!(ops::RemAssign::rem_assign, left, right),
                BinOp::BitXorAssign(_) => {
                    emit_func_call_std!(ops::BitXorAssign::bit_xor_assign, left, right)
                }
                BinOp::BitAndAssign(_) => {
                    emit_func_call_std!(ops::BitAndAssign::bit_and_assign, left, right)
                }
                BinOp::BitOrAssign(_) => {
                    emit_func_call_std!(ops::BitOrAssign::bit_or_assign, left, right)
                }
                BinOp::ShlAssign(_) => emit_func_call_std!(ops::ShlAssign::shl_assign, left, right),
                BinOp::ShrAssign(_) => emit_func_call_std!(ops::ShrAssign::shr_assign, left, right),
                _ => todo!(),
            };

            out_result?.into()
        }
        Expr::Block(block) => {
            assert!(block.attrs.is_empty());
            assert!(block.label.is_none());
            crate::function::process_block(&block.block, output, name_gen)?
        }
        Expr::Break(_brk) => todo!("process_expr -> Expr::Break"),
        Expr::Call(call) => {
            let func = process_expr(&call.func, output, name_gen)?;
            emit_func_call(func, &call.args, output, name_gen)?
        }
        Expr::Cast(_cast) => todo!("process_expr -> Expr::Cast"),
        Expr::Closure(closure) => crate::closure::process_closure(closure, output, name_gen)?,
        Expr::Continue(_cont) => todo!("process_expr -> Expr::Continue"),
        Expr::Field(field) => {
            let base = process_expr(&field.base, output, name_gen)?;
            output.push(quote!{
                let #base = ::embers_transpile::__private::AsExpression::as_expression(&#base, &mut _block_builder)?;
            });

            let field_accessor = field_accessor_for_member(&field.member);

            let out = name_gen.tmp_var("field");
            output.push(quote! {
                let #out = ::embers_transpile::__private::FieldAccess::<::embers_transpile::__private::#field_accessor>::access(&mut _block_builder, #base)?;
            });
            out.into()
        }
        Expr::ForLoop(_for_) => todo!("process_expr -> Expr::ForLoop"),
        Expr::If(_if_) => todo!("process_expr -> Expr::If"),
        Expr::Index(_index) => todo!("process_expr -> Expr::Index"),
        Expr::Lit(lit) => {
            match &lit.lit {
                syn::Lit::Int(lit_int) => {
                    let lit_ty = match lit_int.suffix() {
                        "i32" => quote! { ::embers_transpile::__private::i32 },
                        "u32" => quote! { ::embers_transpile::__private::u32 },
                        "" => quote! { ::embers_transpile::__private::AnyInteger },
                        _ => panic!("unsupported integer literal: {lit_int}"),
                    };

                    let var = name_gen.tmp_var("lit");
                    output.push(quote! {
                        let #var = ::embers_transpile::__private::Literal::<#lit_ty>::new(#lit_int);
                    });
                    ExprOut::from(var)
                }
                syn::Lit::Float(lit_float) => {
                    let lit_ty = match lit_float.suffix() {
                        "f32" => quote! { ::embers_transpile::__private::f32 },
                        "f64" => quote! { ::embers_transpile::__private::f64 },
                        "" => quote! { ::embers_transpile::__private::AnyFloat },
                        _ => panic!("unsupported float literal"),
                    };

                    let var = name_gen.tmp_var("lit");
                    output.push(quote! {
                        let #var = ::embers_transpile::__private::Literal::<#lit_ty>::new(#lit_float);
                    });
                    ExprOut::from(var)
                }
                syn::Lit::Bool(lit_bool) => {
                    let var = name_gen.tmp_var("lit");
                    output.push(quote! {
                        let #var = ::embers_transpile::__private::Literal::<::embers_transpile::__private::bool>::new(#lit_bool);
                    });
                    ExprOut::from(var)
                }
                lit => panic!("unsupported: {lit:?}"),
            }
        }
        Expr::Loop(_loop_) => todo!("process_expr -> Expr::Loop"),
        Expr::Macro(macro_) => crate::function::process_macro(&macro_.mac, output, name_gen)?,
        Expr::MethodCall(call) => {
            /*let mut func_args = vec![];
            for arg in &call.args {
                let arg = process_expr(arg, output, name_gen)?;
                func_args.push(arg);
            }

            let receiver = process_expr(&call.receiver, output, name_gen)?;
            emit_method_call(output, name_gen, receiver, &call.method, &func_args)*/
            todo!();
        }
        Expr::Paren(paren) => {
            assert!(paren.attrs.is_empty());
            process_expr(&paren.expr, output, name_gen)?
        }
        Expr::Path(path) => {
            // note: don't emit the AsExpression here. we might want to borrow this
            // path. or if this is a DeferredDereference, we don't want to emit the load
            // yet. todo: can we handle unit structs here by auto-composing
            // them?

            if path.path.is_ident("self") {
                Ident::new("_self", path.span()).into()
            }
            else {
                path.clone().into()
            }
        }
        Expr::Reference(ref_) => {
            let expr = process_expr(&ref_.expr, output, name_gen)?;
            let out = name_gen.tmp_var("ref");
            output.push(quote! {
                let #out = ::embers_transpile::__private::AsPointer::as_pointer(&#expr, _block_builder.function_builder)?;
            });
            out.into()
        }
        Expr::Repeat(_repeat) => todo!("process_expr -> Expr::Repeat"),
        Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                let out = process_expr(expr, output, name_gen)?;
                output.push(quote! {
                    let #out = ::embers_transpile::__private::AsExpression::as_expression(&#out, &mut _block_builder)?;
                    //_block_builder.function_builder.add_emit(&#out)?;
                    let _return_value = #out.get_naga();
                });
            }
            else {
                output.push(quote! {
                    let _return_value = None;
                });
            }

            let out = name_gen.tmp_var("return");
            output.push(quote! {
                _block_builder.function_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
                    value: _return_value,
                })?;
                let #out = ::embers_transpile::__private::ExpressionHandle::<::embers_transpile::__private::Unit>::empty();
            });
            out.into()
        }
        Expr::Struct(struct_) => {
            let path = &struct_.path;
            assert!(!struct_.dot2_token.is_some());
            assert!(!struct_.rest.is_some());

            let mut fields = TokenBuffer::default();
            for field in &struct_.fields {
                let member = &field.member;
                let colon_token = &field.colon_token;
                let expr = process_expr(&field.expr, output, name_gen)?;
                fields.push(quote!{
                    #member #colon_token ::embers_transpile::__private::AsExpression::as_expression(&#expr, &mut _block_builder)?,
                });
            }

            let out = name_gen.tmp_var("compose");
            output.push(quote! {
                let #out = ::embers_transpile::__private::Compose::compose(
                    &#path {
                        #fields
                    },
                    &mut _block_builder,
                )?;
            });

            out.into()
        }
        Expr::Try(_try_) => todo!("process_expr -> Expr::Try"),
        Expr::Tuple(_tuple) => todo!("process_expr -> Expr::Tuple"),
        Expr::Unary(unary) => {
            match &unary.op {
                UnOp::Deref(_) => {
                    //emit_func_call_std!(ops::Deref::deref, arg)
                    let arg = process_expr(&unary.expr, output, name_gen)?;
                    let out = name_gen.tmp_var("deref");
                    output.push(quote! {
                        let #arg = ::embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _block_builder)?;
                        let #out = ::embers_transpile::__private::Dereference::dereference(&#arg, &mut _block_builder)?;
                    });
                    out.into()
                }
                UnOp::Not(_) => {
                    let out = emit_func_call_std!(ops::Not::not, &unary.expr)?;
                    out.into()
                }
                UnOp::Neg(_) => {
                    let out = emit_func_call_std!(ops::Neg::neg, &unary.expr)?;
                    out.into()
                }
                _ => todo!(),
            }
        }
        Expr::While(_while_) => todo!("process_expr -> Expr::While"),
        _ => panic!("unsupported: {input:?}"),
    };

    Ok(expr_out)
}

fn field_accessor_for_member(member: &Member) -> TokenStream {
    match member {
        syn::Member::Named(named) => {
            let name_lit = ident_to_literal(named);
            quote! { NamedFieldAccessor<{#name_lit}> }
        }
        syn::Member::Unnamed(unnamed) => {
            let index = unnamed.index as usize;
            quote! { UnnamedFieldAccessor<{#index}> }
        }
    }
}
