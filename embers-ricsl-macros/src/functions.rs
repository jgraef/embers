use std::str::FromStr;

use darling::{
    ast::NestedMeta,
    FromAttributes,
    FromMeta,
};
use proc_macro2::TokenStream;
use quote::{
    quote,
    ToTokens,
};
use strum::EnumString;
use syn::{
    Block,
    Expr,
    FnArg,
    Ident,
    ItemFn,
    Meta,
    Pat,
    ReturnType,
    Signature,
    Stmt,
    UnOp, Visibility, ImplItemFn,
};

use crate::{
    args::{
        FnArgs,
        StdFlag, ImplArgs,
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
    let private = args.private();
    let vis = &input.vis;

    let mut output = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    let ident = &input.sig.ident;
    let ident_literal = ident_to_literal(&ident);

    for input in &input.sig.inputs {
        match input {
            FnArg::Receiver(_) => {
                panic!("entrypoint must be an ordinary function");
            }
            FnArg::Typed(pat_type) => {
                let attrs = EntrypointArgAttrs::from_attributes(&pat_type.attrs).unwrap();
                attrs.validate()?;
                match &attrs.builtin {
                    Some(b) => {
                        output.push(quote! { let _binding = Some(#private::naga::Binding::BuiltIn(#b)); });
                    }
                    None => {
                        output.push(quote! { let _binding = None; });
                    }
                };

                let ty = &pat_type.ty;

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let name = ident_to_literal(&pat_ident.ident);
                        let is_mut = pat_ident.mutability.is_some();

                        // todo: parse attributes for bindings

                        let var = &pat_ident.ident;
                        output.push(quote! {
                            let #var = _function_builder.add_input_named::<#ty>(#name, #is_mut, _binding);
                        });
                    }
                    syn::Pat::Wild(_) => {
                        output.push(quote! {
                            _function_builder.add_input_wild::<#ty>();
                        });
                    }
                    _ => panic!("unsupported"),
                }
            }
        }
    }

    match &input.sig.output {
        ReturnType::Default => {}
        ReturnType::Type(_, ty) => {
            panic!("entrypoint can't return anything");
        }
    }

    process_block_inner(&input.block, &mut output, &mut name_gen, args)?;

    let generated = quote! {
        #vis fn #ident() -> #private::Module {
            struct Gen;

            impl #private::FunctionGenerator for Gen {
                type Return = ();

                fn generate(&self, _function_builder: &mut #private::FunctionBuilder) {
                    _function_builder.add_name(#ident_literal);

                    #output
                }
            }

            let mut module_builder = #private::ModuleBuilder::default();
            module_builder.add_entrypoint(Gen);
            module_builder.build()
        }
    };

    Ok(generated)
}

pub fn process_bare_function(input: &ItemFn, args: &FnArgs) -> Result<TokenStream, Error> {
    process_function(&input.vis, &input.sig, &input.block, args)
}

pub fn process_impl_function(input: &ImplItemFn, args: &ImplArgs) -> Result<TokenStream, Error> {
    process_function(&input.vis, &input.sig, &input.block, args)
}

fn process_function<A: StdFlag>(vis: &Visibility, sig: &Signature, block: &Block, args: &A) -> Result<TokenStream, Error> {
    let private = args.private();

    let mut output = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    let ident = &sig.ident;
    let ident_literal = ident_to_literal(&ident);

    output.push(quote! {
        _function_builder.add_name(#ident_literal);
    });

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(_) => {
                output.push(quote! { _function_builder.add_input_receiver(); });
            }
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let name = ident_to_literal(&pat_ident.ident);
                        let is_mut = pat_ident.mutability.is_some();

                        let var = &pat_ident.ident;
                        output.push(quote! {
                            let #var = _function_builder.add_input_named::<#ty>(#name, #is_mut, None);
                        });
                    }
                    syn::Pat::Wild(_) => {
                        output.push(quote! {
                            _function_builder.add_input_wild::<#ty>();
                        });
                    }
                    _ => panic!("unsupported"),
                }
            }
        }
    }

    match &sig.output {
        ReturnType::Type(_, ty) => {
            output.push(quote! {
                _function_builder.add_output::<R>();
            });
        }
        _ => {}
    }

    let (sig, ret) = transform_signature_to_generator(sig, args);

    process_block_inner(block, &mut output, &mut name_gen, args)?;

    let generated = quote! {
        #vis #sig {
            struct Gen<R>(#private::PhantomData<R>);

            impl<R: #private::RicslType> #private::FunctionGenerator for Gen<R> {
                type Return = R;

                fn generate(&self, _function_builder: &mut #private::FunctionBuilder) {
                    #output
                }
            }

            Gen::<#ret>(#private::PhantomData)
        }
    };

    Ok(generated)
}

pub fn transform_signature_to_generator<A: StdFlag>(
    sig: &Signature,
    args: &A,
) -> (TokenStream, TokenStream) {
    let private = args.private();
    let mut has_receiver = false;
    let mut args_first = None;
    let mut args_tail = vec![];

    let ident = &sig.ident;

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(_) => {
                assert!(!has_receiver);
                has_receiver = true;
                args_first = Some(quote! { self: #private::PhantomReceiver<Self> });
            }
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;
                if !has_receiver && args_first.is_none() {
                    args_first = Some(quote!{ _: #private::ExpressionHandle<#ty> });
                }
                else {
                    args_tail.push(quote! { #private::ExpressionHandle<#ty> });
                }
            }
        }
    }

    let args_first = args_first.unwrap_or_else(|| {
        assert!(args_tail.is_empty());
        assert!(!has_receiver);
        quote!{ _: () }
    });

    let args = quote! { #args_first, _: (#(#args_tail),*) };

    let ret = match &sig.output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, ty) => quote! { #ty },
    };

    let sig = quote! {
        fn #ident(#args) -> impl #private::FunctionGenerator<Return = #ret>
    };

    (sig, ret)
}

fn process_block_inner<A: StdFlag>(
    input: &Block,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
    args: &A,
) -> Result<(), Error> {
    for stmt in &input.stmts {
        process_stmt(stmt, output, name_gen, args)?
    }
    Ok(())
}

fn process_stmt<A: StdFlag>(
    input: &Stmt,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
    args: &A
) -> Result<(), Error> {
    let private = args.private();

    match input {
        Stmt::Local(local) => {
            let rhs = if let Some(rhs) = &local.init {
                assert!(rhs.diverge.is_none());
                Some(process_expr(&rhs.expr, output, name_gen, args)?)
            }
            else {
                None
            };

            match &local.pat {
                Pat::Ident(lhs) => {
                    assert!(lhs.by_ref.is_none());
                    assert!(lhs.subpat.is_none());

                    let ident = &lhs.ident;

                    if lhs.mutability.is_some() {
                        todo!("let mut")
                    }
                    else {
                        if let Some(rhs) = rhs {
                            output.push(quote! { let #ident = #private::LetBinding::from_expr(#rhs); });
                        }
                        else {
                            output.push(quote!{ let #ident = #private::LetBinding::unbound(); });
                        }
                    }
                }
                Pat::Wild(_) => {
                    if let Some(rhs) = rhs {
                        output.push(quote! { let _wild = #private::LetBinding::from_expr(#rhs); });
                    }
                }
                _ => panic!("unsupported"),
            }
        }
        Stmt::Item(_) => todo!("process_stmt -> Stmt::Item"),
        Stmt::Expr(expr, _) => {
            let _ = process_expr(expr, output, name_gen, args)?;
        }
        Stmt::Macro(macro_) => {
            output.push(quote!{ #macro_ });
        },
    }

    Ok(())
}

#[must_use]
struct ExprOut(Ident);

impl From<Ident> for ExprOut {
    fn from(ident: Ident) -> Self {
        Self(ident)
    }
}

impl ToTokens for ExprOut {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.0;
        tokens.extend(quote! { #ident })
    }
}

fn process_expr<A: StdFlag>(
    input: &Expr,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
    args: &A
) -> Result<ExprOut, Error> {
    let private = args.private();

    macro_rules! emit_lit {
        ($ty:ident, $variant:ident, $x:expr) => {{
            let var = name_gen.tmp_var("lit");
            let x = $x;
            output.push(quote! {
                let #var = _function_builder.add_expression::<$ty>(
                    #private::naga::Expression::Literal(
                        #private::naga::Literal::$variant(#x)
                    )
                );
            });
            ExprOut::from(var)
        }};
    }

    macro_rules! emit_func_call {
        ($func_path:expr, $args:expr) => {{
            let out = name_gen.tmp_var("call");
            let func_path = $func_path;
            let args: Vec<ExprOut> = $args.into();
            let (args_self, args_tail, args_handles) = if let Some(first) = args.get(0) {
                (
                    quote!{ #first.into() },
                    &args[1..],
                    quote!{ [#(#args.handle()),*].into_iter().flatten().collect() },
                )
            }
            else {
                (
                    quote!{ () },
                    &[] as &[ExprOut],
                    quote!{ vec![] },
                )
            };

            output.push(quote! {
                let _func = &#func_path;
                let _gen = _func(#args_self, (#(#args_tail),*));
                let #out = _function_builder.add_call(&_func, _gen, #args_handles);
            });

            ExprOut::from(out)
        }};
    }
    macro_rules! emit_func_call_std {
        ($func_path:path, $($args:expr),*) => {{
            let args = vec![$($args),*];
            emit_func_call!(quote!{#private::rstd::$func_path}, args)
        }};
    }
    macro_rules! emit_func_call_expr {
        ($func_expr:expr, $args:expr) => {{
            let func_expr = $func_expr;
            output.push(quote! {
                let _func = #func_expr.constant().expect("not a function").f;
            });
            emit_func_call!(quote!{ _func }, $args)
        }};
    }

    let expr_out = match input {
        Expr::Assign(assign) => todo!("process_expr -> Expr::Assign"),
        Expr::Binary(binary) => todo!("process_expr -> Expr::Binary"),
        Expr::Block(block) => {
            assert!(block.attrs.is_empty());
            assert!(block.label.is_none());
            let mut block_output = TokenBuffer::default();
            let var = name_gen.tmp_var("block");
            process_block_inner(&block.block, &mut block_output, name_gen, args)?;
            output.push(quote! {
                let #var = {
                    #block_output
                };
            });
            var.into()
        }
        Expr::Break(brk) => todo!("process_expr -> Expr::Break"),
        Expr::Call(call) => {
            let func = &call.func;
            let func = process_expr(func, output, name_gen, args)?;
            let mut func_args = vec![];
            for arg in &call.args {
                let arg = process_expr(arg, output, name_gen, args)?;
                func_args.push(arg);
            }
            emit_func_call_expr!(func, func_args)
        }
        Expr::Cast(cast) => todo!("process_expr -> Expr::Cast"),
        Expr::Closure(closure) => todo!("process_expr -> Expr::Closure"),
        Expr::Continue(cont) => todo!("process_expr -> Expr::Continue"),
        Expr::Field(field) => todo!("process_expr -> Expr::Field"),
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
        Expr::MethodCall(call) => todo!("process_expr -> Expr::MethodCall"),
        Expr::Paren(paren) => {
            assert!(paren.attrs.is_empty());
            process_expr(&paren.expr, output, name_gen, args)?
        }
        Expr::Path(path) => {
            let var = name_gen.tmp_var("path");
            output.push(quote! { let #var = ::#private::IntoExpressionHandle::into_expr(#path, _function_builder).unwrap(); });
            var.into()
            //path.path.clone().into()
        }
        Expr::Repeat(repeat) => todo!("process_expr -> Expr::Repeat"),
        Expr::Return(ret) => todo!("process_expr -> Expr::Return"),
        Expr::Struct(strct) => todo!("process_expr -> Expr::Struct"),
        Expr::Try(try_) => todo!("process_expr -> Expr::Try"),
        Expr::Tuple(tuple) => todo!("process_expr -> Expr::Tuple"),
        Expr::Unary(unary) => {
            let arg = process_expr(&unary.expr, output, name_gen, args)?;
            let out = match &unary.op {
                UnOp::Deref(_) => {
                    emit_func_call_std!(ops::Deref::deref, arg)
                }
                UnOp::Not(_) => {
                    emit_func_call_std!(ops::Not::not, arg)
                }
                UnOp::Neg(_) => {
                    emit_func_call_std!(ops::Neg::neg, arg)
                }
                _ => todo!(),
            };
            out.into()
        }
        Expr::While(while_) => todo!("process_expr -> Expr::While"),
        _ => panic!("unsupported: {input:?}"),
    };

    Ok(expr_out)
}
