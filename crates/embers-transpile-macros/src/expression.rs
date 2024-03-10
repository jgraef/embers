use proc_macro2::{
    Span,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use syn::{
    BinOp,
    Expr,
    Ident,
    Member,
    Path,
    UnOp,
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
    Path(Path),
}

impl From<Path> for ExprOut {
    fn from(path: Path) -> Self {
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
    fn emit_func_call(
        output: &mut TokenBuffer,
        name_gen: &mut NameGen,
        func: impl ToTokens,
        args: &[ExprOut],
    ) -> ExprOut {
        let out = name_gen.tmp_var("callres");
        let mut arg_binds = vec![];
        let mut arg_names = vec![];

        for (i, arg) in args.iter().enumerate() {
            let bind = name_gen.tmp_var("arg");
            arg_binds.push(quote!{
                let #bind = ::embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _block_builder)?;
            });
            if i == 0 {
                arg_binds.push(quote! {
                    let #bind = ::embers_transpile::__private::Into::into(#bind);
                });
            }
            arg_names.push(bind);
        }

        output.push(quote! {
            let #out = {
                #(#arg_binds)*
                let _gen = #func(#(#arg_names),*);
                ::embers_transpile::__private::GenerateCall::call(&_gen, &mut _block_builder)?
            };
        });

        ExprOut::from(out)
    }

    fn emit_method_call(
        output: &mut TokenBuffer,
        name_gen: &mut NameGen,
        receiver: ExprOut,
        method: &Ident,
        args: &[ExprOut],
    ) -> ExprOut {
        let out = name_gen.tmp_var("callres");
        let mut arg_binds = vec![];
        let mut arg_names = vec![];

        for (i, arg) in args.iter().enumerate() {
            let bind = name_gen.tmp_var("arg");
            arg_binds.push(quote!{
                let #bind = ::embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _block_builder)?;
            });
            if i == 0 {
                arg_binds.push(quote! {
                    let #bind = ::embers_transpile::__private::Into::into(#bind);
                });
            }
            arg_names.push(bind);
        }

        output.push(quote! {
            let #out = {
                #(#arg_binds)*
                let #receiver = ::embers_transpile::__private::AsExpression::as_expression(&#receiver, &mut _block_builder)?;
                let _gen = ::embers_transpile::__private::PhantomReceiverPointer::from(#receiver).#method((#(#args),*));
                ::embers_transpile::__private::GenerateCall::call(&_gen, _block_builder)?
            };
        });

        ExprOut::from(out)
    }

    macro_rules! emit_func_call_std {
        ($func_path:path, $($args:expr),*) => {{
            let args = &[$($args),*];
            emit_func_call(output, name_gen, quote!{ ::embers_transpile::__private::shader_std::$func_path }, args)
        }};
    }

    fn emit_func_call_to_expr(
        output: &mut TokenBuffer,
        name_gen: &mut NameGen,
        func: ExprOut,
        args: &[ExprOut],
    ) -> ExprOut {
        emit_func_call(output, name_gen, quote! { #func }, args)
    }

    let expr_out = match input {
        Expr::Assign(_assign) => todo!("process_expr -> Expr::Assign"),
        Expr::Binary(binary) => {
            let left = process_expr(&binary.left, output, name_gen)?;
            let right = process_expr(&binary.right, output, name_gen)?;

            let out = match &binary.op {
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

            out.into()
        }
        Expr::Block(block) => {
            assert!(block.attrs.is_empty());
            assert!(block.label.is_none());
            let mut block_output = TokenBuffer::default();
            let var = name_gen.tmp_var("block");
            crate::function::process_block(&block.block, &mut block_output, name_gen)?
        }
        Expr::Break(_brk) => todo!("process_expr -> Expr::Break"),
        Expr::Call(call) => {
            let func = &call.func;
            let func = process_expr(func, output, name_gen)?;
            let mut func_args = vec![];
            for arg in &call.args {
                let arg = process_expr(arg, output, name_gen)?;
                func_args.push(arg);
            }
            emit_func_call_to_expr(output, name_gen, func, &func_args)
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
            // todo: don't emit the IntoExpressionHandle yet. we might want to borrow this
            // path

            //let var = name_gen.tmp_var("path");
            //output.push(quote! { let #var =
            // #private::IntoExpressionHandle::into_expr(#path, _function_builder).unwrap();
            // }); var.into()

            if path.path.is_ident("self") {
                Ident::new("_self", Span::call_site()).into()
            }
            else {
                path.path.clone().into()
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
                    let arg = process_expr(&unary.expr, output, name_gen)?;
                    let out = emit_func_call_std!(ops::Not::not, arg);
                    out.into()
                }
                UnOp::Neg(_) => {
                    let arg = process_expr(&unary.expr, output, name_gen)?;
                    let out = emit_func_call_std!(ops::Neg::neg, arg);
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
