use std::str::FromStr;

use darling::{
    ast::NestedMeta,
    FromAttributes,
    FromMeta,
};
use proc_macro2::{
    Span,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use strum::EnumString;
use syn::{
    BinOp,
    Block,
    Expr,
    FnArg,
    Ident,
    ImplItemFn,
    ItemFn,
    Macro,
    Member,
    Meta,
    Pat,
    Path,
    Receiver,
    ReturnType,
    Signature,
    Stmt,
    Type,
    UnOp,
    Visibility,
};

use crate::{
    args::{
        FnArgs,
        ImplArgs,
    },
    error::Error,
    utils::{
        ident_to_literal,
        NameGen,
        TokenBuffer,
    },
};

#[derive(Debug, EnumString)]
#[strum(serialize_all = "snake_case")]
enum BuiltinType {
    Position { invariant: bool },
    ViewIndex,
    BaseInstance,
    BaseVertex,
    ClipDistance,
    CullDistance,
    InstanceIndex,
    PointSize,
    VertexIndex,
    FragDepth,
    PointCoord,
    FrontFacing,
    PrimitiveIndex,
    SampleIndex,
    SampleMask,
    GlobalInvocationId,
    LocalInvocationId,
    LocalInvocationIndex,
    WorkGroupId,
    WorkGroupSize,
    NumWorkGroups,
}

impl ToTokens for BuiltinType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let out = match self {
            BuiltinType::Position { invariant } => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::Position { #invariant } }
            }
            BuiltinType::ViewIndex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::ViewIndex }
            }
            BuiltinType::BaseInstance => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::BaseInstance }
            }
            BuiltinType::BaseVertex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::BaseVertex }
            }
            BuiltinType::ClipDistance => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::ClipDistance }
            }
            BuiltinType::CullDistance => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::CullDistance }
            }
            BuiltinType::InstanceIndex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::InstanceIndex }
            }
            BuiltinType::PointSize => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::PointSize }
            }
            BuiltinType::VertexIndex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::VertexIndex }
            }
            BuiltinType::FragDepth => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::FragDepth }
            }
            BuiltinType::PointCoord => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::PointCoord }
            }
            BuiltinType::FrontFacing => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::FrontFacing }
            }
            BuiltinType::PrimitiveIndex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::PrimitiveIndex }
            }
            BuiltinType::SampleIndex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::SampleIndex }
            }
            BuiltinType::SampleMask => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::SampleMask }
            }
            BuiltinType::GlobalInvocationId => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::GlobalInvocationId }
            }
            BuiltinType::LocalInvocationId => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::LocalInvocationId }
            }
            BuiltinType::LocalInvocationIndex => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::LocalInvocationIndex }
            }
            BuiltinType::WorkGroupId => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::WorkGroupId }
            }
            BuiltinType::WorkGroupSize => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::WorkGroupSize }
            }
            BuiltinType::NumWorkGroups => {
                quote! { ::embers_ricsl::__private::naga::BuiltIn::NumWorkGroups }
            }
        };
        tokens.extend(out);
    }
}

impl FromMeta for BuiltinType {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        assert_eq!(items.len(), 1);

        match &items[0] {
            NestedMeta::Meta(meta) => {
                match meta {
                    Meta::Path(path) => {
                        let ident = path
                            .get_ident()
                            .ok_or_else(|| darling::Error::custom("unexpected path"))?
                            .to_string();
                        Ok(BuiltinType::from_str(&ident).map_err(|e| darling::Error::custom(e))?)
                    }
                    Meta::List(list) => {
                        todo!()
                    }
                    Meta::NameValue(_) => todo!(),
                }
            }
            NestedMeta::Lit(_) => Err(darling::Error::custom("expected path")),
        }
    }
}

#[derive(Debug, FromMeta)]
struct Binding {
    #[darling(default)]
    group: usize,
    id: usize,
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(ricsl))]
struct EntrypointArgAttrs {
    builtin: Option<BuiltinType>,
    binding: Option<Binding>,
}

impl EntrypointArgAttrs {
    pub fn validate(&self) -> Result<(), Error> {
        if self.builtin.is_some() && self.binding.is_some() {
            panic!("argument can't be builtin and binding at the same time");
        }
        Ok(())
    }
}

pub fn process_entrypoint(input: &ItemFn, args: &FnArgs) -> Result<TokenStream, Error> {
    let vis = &input.vis;
    let sig = &input.sig;
    let block = &input.block;
    let ident = &input.sig.ident;
    let ret = quote! { () };

    let body = generate_function_body(sig, ret, block, true)?;

    let generated = quote! {
        #vis fn #ident() -> ::embers_transpile::__private::Result<::embers_transpile::__private::Module, ::embers_transpile::__private::BuilderError> {
            let mut module_builder = ::embers_transpile::__private::ModuleBuilder::default();
            module_builder.add_entrypoint(
                ::embers_transpile::__private::EntrypointGenerator::new(
                    |mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder| {
                        #body
                    },
                )
            )?;
            let module = module_builder.build();
            Ok(module)
        }
    };

    Ok(generated)
}

pub fn process_bare_function(input: &ItemFn, args: &FnArgs) -> Result<TokenStream, Error> {
    process_function(&input.vis, &input.sig, &input.block, args.inline)
}

pub fn process_impl_function(input: &ImplItemFn, _args: &ImplArgs) -> Result<TokenStream, Error> {
    process_function(&input.vis, &input.sig, &input.block, false)
}

fn process_function(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
    inline: bool,
) -> Result<TokenStream, Error> {
    let (sig_transformed, ret) = transform_signature_to_generator(sig);

    let generated = if inline {
        let body = generate_function_body_inline(sig, ret, block)?;
        quote! {
            #vis #sig_transformed {
                ::embers_transpile::__private::InlineCallGenerator::new(
                    move |mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder| {
                        #body
                    },
                )
            }
        }
    }
    else {
        let body = generate_function_body(sig, ret, block, false)?;
        let call = generate_function_call(sig)?;

        quote! {
            #vis #sig_transformed {
                ::embers_transpile::__private::CallGenerator::new(
                    |mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder| {
                        #body
                    },
                    move |
                        mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder,
                        _func_handle: ::embers_transpile::__private::TypeHandle,
                    | {
                        #call
                    },
                )
            }
        }
    };

    Ok(generated)
}

pub fn transform_signature_to_generator(sig: &Signature) -> (TokenStream, TokenStream) {
    let mut has_receiver = false;
    let mut args = vec![];

    let ident = &sig.ident;

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                assert!(!has_receiver);
                has_receiver = true;

                let ty = &receiver.ty;
                let ty = if receiver.reference.is_some() {
                    quote! { ::embers_transpile::__private::PhantomReceiverPointer<Self, { ::embers_transpile::__private::AddressSpace::Private }> }
                }
                else {
                    quote! { ::embers_transpile::__private::PhantomReceiver<Self> }
                };

                args.push(quote! { self: #ty });
            }
            FnArg::Typed(pat_type) => {
                let pat = &pat_type.pat;
                let ty = &pat_type.ty;
                // todo: accept impl AsExpressionHandle
                args.push(quote! { #pat: ::embers_transpile::__private::ExpressionHandle<#ty> });
            }
        }
    }

    let ret = match &sig.output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, ty) => quote! { #ty },
    };

    let sig = quote! {
        fn #ident(#(#args),*) -> impl ::embers_transpile::__private::GenerateCall<Return = #ret>
    };

    (sig, ret)
}

fn generate_function_body(
    sig: &Signature,
    ret: TokenStream,
    block: &Block,
    entrypoint: bool,
) -> Result<TokenStream, Error> {
    let mut body = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    let ident = &sig.ident;
    let ident_literal = ident_to_literal(&ident);

    body.push(quote! {
        _function_builder.add_name(#ident_literal);
    });

    // take inputs
    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                assert!(!entrypoint);
                assert!(receiver.colon_token.is_none());

                let ty = &receiver.ty;
                let ty = if receiver.reference.is_some() {
                    // todo: what address space do we use? we could abuse lifetimes for this.
                    quote! { ::embers_transpile::__private::Pointer<Self, { ::embers_transpile::__private::AddressSpace::Private }> }
                }
                else {
                    quote! { Self }
                };

                body.push(quote! { let _self = _function_builder.add_input_receiver::<#ty>()?; });
            }
            FnArg::Typed(pat_type) => {
                if entrypoint {
                    let attrs = EntrypointArgAttrs::from_attributes(&pat_type.attrs).unwrap();
                    attrs.validate()?;
                    match &attrs.builtin {
                        Some(b) => {
                            body.push(
                                quote! { let _binding = Some(::embers_transpile::__private::naga::Binding::BuiltIn(#b)); },
                            );
                        }
                        None => {
                            body.push(quote! { let _binding = None; });
                        }
                    };
                }
                else {
                    body.push(quote! { let _binding = None; });
                }

                let ty = &pat_type.ty;

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let name = ident_to_literal(&pat_ident.ident);
                        let is_mut = pat_ident.mutability.is_some();

                        let var = &pat_ident.ident;
                        body.push(quote! {
                            let #var = _function_builder.add_input_named::<#ty>(#name, #is_mut, _binding)?;
                        });
                    }
                    syn::Pat::Wild(_) => {
                        body.push(quote! {
                            _function_builder.add_input_wild::<#ty>()?;
                        });
                    }
                    _ => panic!("unsupported"),
                }
            }
        }
    }

    match &sig.output {
        ReturnType::Type(_, ty) => {
            body.push(quote! {
                _function_builder.add_output::<#ty>()?;
            });
        }
        _ => {}
    }

    let result_var = process_block_inner(block, &mut body, &mut name_gen)?;

    // make sure the return value type matches
    body.push(quote! {
        let #result_var = ::embers_transpile::__private::AsExpression::as_expression(&#result_var, &mut _function_builder)?;
        let #result_var: ::embers_transpile::__private::ExpressionHandle<#ret> = #result_var;
    });

    // return
    body.push(quote! {
        _function_builder.add_emit(&#result_var)?;
        _function_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
            value: #result_var.get_handle(),
        });
        ::embers_transpile::__private::Ok::<(), ::embers_transpile::__private::BuilderError>(())
    });

    Ok(body.into_token_stream())
}

fn generate_function_call(sig: &Signature) -> Result<TokenStream, Error> {
    let mut name_gen = NameGen::default();
    let mut arg_names = vec![];
    //let mut arg_bindings = vec![];

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                assert!(receiver.colon_token.is_none());
                //arg_bindings.push(quote!{
                //    let _self = ::embers_transpile::__private::AsExpression::as_expression(&self, &mut _function_builder)?;
                //});
                arg_names.push(quote! { self });
            }
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let var = &pat_ident.ident;
                        //let bind = name_gen.tmp_var("arg");
                        //arg_bindings.push(quote!{
                        //    let #bind = ::embers_transpile::__private::AsExpression::as_expression(&#var, &mut _function_builder)?;
                        //});
                        arg_names.push(quote! { #var });
                    }
                    syn::Pat::Wild(_) => {
                        // nothing to do here, except make sure the generated
                        // function ignores this argument.
                    }
                    _ => panic!("unsupported"),
                }
            }
        }
    }

    Ok(quote! {
        {
            let _args = [
                #(::embers_transpile::__private::AsExpression::as_expression(&#arg_names, &mut _function_builder)?.get_handle()),*
            ].into_iter().flatten();
            _function_builder.add_call(
                _func_handle,
                _args,
            )
        }
    })
}


fn generate_function_body_inline(
    sig: &Signature,
    ret: TokenStream,
    block: &Block,
) -> Result<TokenStream, Error> {
    let mut body = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    // the inputs already have the right names and will be moved into to generating closure

    let result_var = process_block_inner(block, &mut body, &mut name_gen)?;

    // make sure the return value type matches
    body.push(quote! {
        let #result_var = ::embers_transpile::__private::AsExpression::as_expression(&#result_var, &mut _function_builder)?;
        let #result_var: ::embers_transpile::__private::ExpressionHandle<#ret> = #result_var;
        // should we emit here?
        //_function_builder.add_emit(&#result_var)?;
        ::embers_transpile::__private::Ok::<_, ::embers_transpile::__private::BuilderError>(#result_var)
    });

    Ok(quote!{
        {
            #body
        }
    })
}


fn implicit_unit(
    expr: Option<ExprOut>,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> ExprOut {
    expr.unwrap_or_else(|| {
        let var = name_gen.tmp_var("implicit_unit");
        output.push(quote! {
            let #var = ::embers_transpile::__private::ExpressionHandle::<()>::from_empty();
        });
        ExprOut::from(var)
    })
}

fn process_block_inner(
    input: &Block,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    let mut result = None;
    for stmt in &input.stmts {
        result = Some(process_stmt(stmt, output, name_gen)?);
    }

    let result = implicit_unit(result, output, name_gen);

    Ok(result)
}

fn process_stmt(
    input: &Stmt,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    let result = match input {
        Stmt::Local(local) => {
            let rhs = if let Some(rhs) = &local.init {
                assert!(rhs.diverge.is_none());
                Some(process_expr(&rhs.expr, output, name_gen)?)
            }
            else {
                None
            };

            let lhs = LetLhs::from_pat(&local.pat);

            let lhs_ty = if let Some(ty) = lhs.ty {
                quote! { #ty }
            }
            else {
                quote! { _ }
            };

            if let Some(ident) = lhs.ident {
                let ident_literal = ident_to_literal(ident);

                if lhs.is_mut {
                    let init = if let Some(rhs) = &rhs {
                        quote! { Some(#rhs) }
                    }
                    else {
                        quote! { None }
                    };

                    output.push(quote! {
                        let #ident = _function_builder.add_local_variable::<#lhs_ty>(#ident_literal, #init)?;
                    });
                }
                else {
                    if let Some(rhs) = &rhs {
                        output.push(quote! {
                             _function_builder.name_expression(#ident_literal, #rhs.clone())?;
                             let #ident = ::embers_transpile::__private::LetBinding::<#lhs_ty>::from_expr(#rhs);
                        });
                    }
                    else {
                        output.push(
                            quote! { let #ident = ::embers_transpile::__private::LetBinding::<#lhs_ty>::unbound(); },
                        );
                    }
                }
            }

            None
        }
        Stmt::Item(_) => todo!("process_stmt -> Stmt::Item"),
        Stmt::Expr(expr, _) => Some(process_expr(expr, output, name_gen)?),
        Stmt::Macro(macro_) => Some(process_macro(&macro_.mac, output, name_gen)?),
    };

    let result = implicit_unit(result, output, name_gen);

    Ok(result)
}

#[derive(Debug, Default)]
struct LetLhs<'a> {
    is_mut: bool,
    ident: Option<&'a Ident>,
    ty: Option<&'a Type>,
}

impl<'a> LetLhs<'a> {
    pub fn from_pat(pat: &'a Pat) -> Self {
        match pat {
            Pat::Ident(pat_ident) => {
                assert!(pat_ident.by_ref.is_none());
                assert!(pat_ident.subpat.is_none());
                LetLhs {
                    is_mut: pat_ident.mutability.is_some(),
                    ident: Some(&pat_ident.ident),
                    ty: None,
                }
            }
            Pat::Type(pat_type) => {
                let mut lhs = LetLhs::from_pat(&pat_type.pat);
                assert!(lhs.ty.is_none());
                lhs.ty = Some(&pat_type.ty);
                lhs
            }
            Pat::Wild(_) => LetLhs::default(),
            _ => todo!("unsupported pattern"),
        }
    }
}

fn process_macro(
    macro_: &Macro,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    let path = &macro_.path;
    let var = name_gen.tmp_var("macro");

    fn is_intrinsic(path: &Path) -> bool {
        if path.leading_colon.is_none() {
            return false;
        }

        let mut it = path.segments.iter();
        let mut matches = |s| {
            it.next()
                .map(|seg| seg.ident.to_string() == s)
                .unwrap_or_default()
        };
        matches("embers_transpile")
            && matches("__private")
            && matches("intrinsic")
            && it.next().is_none()
    }

    if is_intrinsic(&macro_.path) {
        let tokens = &macro_.tokens;
        output.push(quote! {
            let #var = {
                // just so that we have a proper name and no warnings. we also might want to change the name of the _function_builder variable later.
                #[allow(unused_variables)]
                let mut function_builder = &mut _function_builder;

                #tokens
            };
        });
    }
    else if path.is_ident("todo") {
        // just put this inplace, so we panic during code generation
        output.push(quote! {
            #macro_;
            let #var = ::embers_transpile::__private::ExpressionHandle::<()>::from_phantom();
        });
    }
    else {
        // todo: #var will not be declared
    }

    Ok(var.into())
}

#[must_use]
enum ExprOut {
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

fn process_expr(
    input: &Expr,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    macro_rules! emit_lit {
        ($ty:ident, $variant:ident, $x:expr) => {{
            let var = name_gen.tmp_var("lit");
            let x = $x;
            output.push(quote! {
                let #var = _function_builder.add_expression::<$ty>(
                    ::embers_transpile::__private::naga::Expression::Literal(
                        ::embers_transpile::__private::naga::Literal::$variant(#x)
                    )
                );
            });
            ExprOut::from(var)
        }};
    }

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
                let #bind = ::embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _function_builder)?;
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
                ::embers_transpile::__private::GenerateCall::call(&_gen, _function_builder)?
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
                let #bind = ::embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _function_builder)?;
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
                let #receiver = ::embers_transpile::__private::AsExpression::as_expression(&#receiver, &mut _function_builder)?;
                let _gen = ::embers_transpile::__private::PhantomReceiverPointer::from(#receiver).#method((#(#args),*));
                ::embers_transpile::__private::GenerateCall::call(&_gen, _function_builder)?
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
        emit_func_call(output, name_gen, quote! { &#func }, args)
    }

    let expr_out = match input {
        Expr::Assign(assign) => todo!("process_expr -> Expr::Assign"),
        Expr::Binary(binary) => {
            let left = process_expr(&binary.left, output, name_gen)?;
            let right = process_expr(&binary.right, output, name_gen)?;
            //output.push(quote!{
            //    let #left =
            // ::embers_transpile::__private::AsExpression::as_expression(&#left, &mut
            // _function_builder)?;    let #right =
            // ::embers_transpile::__private::AsExpression::as_expression(&#right, &mut
            // _function_builder)?;
            //});

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
            let block_result = process_block_inner(&block.block, &mut block_output, name_gen)?;
            output.push(quote! {
                let #var = {
                    #block_output
                    #block_result
                };
            });
            var.into()
        }
        Expr::Break(brk) => todo!("process_expr -> Expr::Break"),
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
        Expr::Cast(cast) => todo!("process_expr -> Expr::Cast"),
        Expr::Closure(closure) => todo!("process_expr -> Expr::Closure"),
        Expr::Continue(cont) => todo!("process_expr -> Expr::Continue"),
        Expr::Field(field) => {
            let base = process_expr(&field.base, output, name_gen)?;
            output.push(quote!{
                let #base = ::embers_transpile::__private::AsExpression::as_expression(&#base, &mut _function_builder)?;
            });

            let field_accessor = field_accessor_for_member(&field.member);

            let out = name_gen.tmp_var("field");
            output.push(quote! {
                let #out = ::embers_transpile::__private::Field::new::<{::embers_transpile::__private::FieldAccessor::#field_accessor}>(#base);
                //let #out = #private::AsExpression::as_expression(&_field, _function_builder).unwrap();
            });
            out.into()
        }
        Expr::ForLoop(for_) => todo!("process_expr -> Expr::ForLoop"),
        Expr::If(if_) => todo!("process_expr -> Expr::If"),
        Expr::Index(index) => todo!("process_expr -> Expr::Index"),
        Expr::Lit(lit) => {
            match &lit.lit {
                syn::Lit::Int(x) => {
                    match x.suffix() {
                        "i32" | "" => emit_lit!(i32, I32, x),
                        "u32" => emit_lit!(u32, U32, x),
                        suffix => panic!("unsupported integer literal suffix: {suffix}"),
                    }
                }
                syn::Lit::Float(x) => {
                    match x.suffix() {
                        "f32" | "" => emit_lit!(f32, F32, x),
                        "f64" => emit_lit!(f64, F64, x),
                        suffix => panic!("unsupported float literal suffix: {suffix}"),
                    }
                }
                syn::Lit::Bool(x) => {
                    emit_lit!(bool, Bool, x)
                }
                lit => panic!("unsupported: {lit:?}"),
            }
        }
        Expr::Loop(loop_) => todo!("process_expr -> Expr::Loop"),
        Expr::Macro(macro_) => process_macro(&macro_.mac, output, name_gen)?,
        Expr::MethodCall(call) => {
            let mut func_args = vec![];
            for arg in &call.args {
                let arg = process_expr(arg, output, name_gen)?;
                func_args.push(arg);
            }

            let receiver = process_expr(&call.receiver, output, name_gen)?;
            emit_method_call(output, name_gen, receiver, &call.method, &func_args)
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
                let #out = ::embers_transpile::__private::AsPointer::as_pointer(&#expr, _function_builder)?;
            });
            out.into()
        }
        Expr::Repeat(repeat) => todo!("process_expr -> Expr::Repeat"),
        Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                let out = process_expr(expr, output, name_gen)?;
                output.push(quote! {
                    let #out = ::embers_transpile::__private::AsExpression::as_expression(&#out, &mut _function_builder)?;
                    _function_builder.add_emit(&#out)?;
                    let _return_value = #out.get_handle();
                });
            }
            else {
                output.push(quote! {
                    let _return_value = None;
                });
            }

            let out = name_gen.tmp_var("return");
            output.push(quote! {
                _function_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
                    value: _return_value,
                });
                let #out = ::embers_transpile::__private::ExpressionHandle::<()>::from_empty();
            });
            out.into()
        }
        Expr::Struct(strct) => {
            let ty = &strct.path;

            output.push(quote! {
                let _struct_ty = _function_builder.module_builder.get_type_by_id_or_add_it::<#ty>()?.try_get_data()?;
                let mut _field_exprs: ::embers_transpile::__private::Vec<::embers_transpile::__private::naga::Handle<::embers_transpile::__private::naga::Expression>> = vec![];
            });

            for field in &strct.fields {
                let expr_var = process_expr(&field.expr, output, name_gen)?;
                let field_accessor = field_accessor_for_member(&field.member);
                output.push(quote! {
                    let _field: ::embers_transpile::__private::ExpressionHandle<<#ty as ::embers_transpile::__private::FieldAccess::<{::embers_transpile::__private::FieldAccessor::#field_accessor}>>::Type> = #expr_var;
                    if let Some(handle) = _field.get_handle() {
                        _field_exprs.push(handle);
                    }
                });
            }

            let out = name_gen.tmp_var("compose");
            output.push(quote! {
                let #out = _function_builder.add_expression::<#ty>(::embers_transpile::__private::naga::Expression::Compose {
                    ty: _struct_ty,
                    components: _field_exprs,
                });
                _function_builder.add_emit(&#out)?;
            });

            out.into()
        }
        Expr::Try(try_) => todo!("process_expr -> Expr::Try"),
        Expr::Tuple(tuple) => todo!("process_expr -> Expr::Tuple"),
        Expr::Unary(unary) => {
            match &unary.op {
                UnOp::Deref(_) => {
                    //emit_func_call_std!(ops::Deref::deref, arg)
                    let arg = process_expr(&unary.expr, output, name_gen)?;
                    let out = name_gen.tmp_var("deref");
                    output.push(quote! {
                        let #arg = ::embers_transpile::__private::AsExpression::as_expression(&#arg, &mut _function_builder)?;
                        let #out = ::embers_transpile::__private::Dereference::dereference(&#arg, &mut _function_builder)?;
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
        Expr::While(while_) => todo!("process_expr -> Expr::While"),
        _ => panic!("unsupported: {input:?}"),
    };

    Ok(expr_out)
}

fn field_accessor_for_member(member: &Member) -> TokenStream {
    match member {
        syn::Member::Named(named) => {
            let name_lit = ident_to_literal(named);
            quote! { Named(#name_lit) }
        }
        syn::Member::Unnamed(unnamed) => {
            let index = unnamed.index as usize;
            quote! { Unnamed(#index) }
        }
    }
}
