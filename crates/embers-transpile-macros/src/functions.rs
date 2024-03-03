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
                        output.push(
                            quote! { let _binding = Some(::embers_transpile::__private::naga::Binding::BuiltIn(#b)); },
                        );
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

    process_block_inner(&input.block, &mut output, &mut name_gen)?;

    let generated = quote! {
        #vis fn #ident() -> ::embers_transpile::__private::std::result::Result<::embers_transpile::__private::Module, ::embers_transpile::__private::BuilderError> {
            let mut module_builder = ::embers_transpile::__private::ModuleBuilder::default();
            module_builder.add_entrypoint(|mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder| {
                _function_builder.add_name(#ident_literal);
                #output
                ::embers_transpile::__private::std::result::Result::Ok(())
            })?;
            let module = module_builder.build();
            Ok(module)
        }
    };

    Ok(generated)
}

pub fn process_bare_function(input: &ItemFn, _args: &FnArgs) -> Result<TokenStream, Error> {
    process_function(&input.vis, &input.sig, &input.block)
}

pub fn process_impl_function(input: &ImplItemFn, _args: &ImplArgs) -> Result<TokenStream, Error> {
    process_function(&input.vis, &input.sig, &input.block)
}

fn process_function(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
) -> Result<TokenStream, Error> {
    let mut output = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    let ident = &sig.ident;
    let ident_literal = ident_to_literal(&ident);

    output.push(quote! {
        _function_builder.add_name(#ident_literal);
    });

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                assert!(receiver.colon_token.is_none());
                let ty = &receiver.ty;
                output.push(quote! { let _self = _function_builder.add_input_receiver::<#ty>(); });
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
                _function_builder.add_output::<#ty>();
            });
        }
        _ => {}
    }

    let (sig, ret) = transform_signature_to_generator(sig);

    let result_var = process_block_inner(block, &mut output, &mut name_gen)?;

    if let Some(result_var) = result_var {
        output.push(quote! {
            _function_builder.add_emit(&#result_var)?;
            _function_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
                value: #result_var.get_handle(),
            });
        })
    }

    let generated = quote! {
        #vis #sig {
            |mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder| {
                #output
                ::embers_transpile::__private::std::result::Result::Ok::<(), ::embers_transpile::__private::BuilderError>(())
            }
        }
    };

    Ok(generated)
}

pub fn transform_signature_to_generator(sig: &Signature) -> (TokenStream, TokenStream) {
    let mut has_receiver = false;
    let mut args_first = None;
    let mut args_tail = vec![];

    let ident = &sig.ident;

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(_) => {
                assert!(!has_receiver);
                has_receiver = true;
                args_first =
                    Some(quote! { self: ::embers_transpile::__private::PhantomReceiver<Self> });
            }
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;
                if !has_receiver && args_first.is_none() {
                    args_first =
                        Some(quote! { _: ::embers_transpile::__private::ExpressionHandle<#ty> });
                }
                else {
                    args_tail.push(quote! { ::embers_transpile::__private::ExpressionHandle<#ty> });
                }
            }
        }
    }

    let args_first = args_first.unwrap_or_else(|| {
        assert!(args_tail.is_empty());
        assert!(!has_receiver);
        quote! { _: () }
    });

    let args = quote! { #args_first, _: (#(#args_tail),*) };

    let ret = match &sig.output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, ty) => quote! { #ty },
    };

    let sig = quote! {
        fn #ident(#args) -> impl ::embers_transpile::__private::FunctionGenerator<#ret>
    };

    (sig, ret)
}

fn process_block_inner(
    input: &Block,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<Option<ExprOut>, Error> {
    let mut result = None;
    for stmt in &input.stmts {
        result = process_stmt(stmt, output, name_gen)?;
    }
    Ok(result)
}

fn process_stmt(
    input: &Stmt,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<Option<ExprOut>, Error> {
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

    if path.is_ident("embers_transpile_intrinsic") {
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
        func_ref: impl ToTokens,
        args: &[ExprOut],
    ) -> ExprOut {
        let out = name_gen.tmp_var("callres");

        if args.is_empty() {
            output.push(quote! {
                let _args_handles = ::embers_transpile::__private::std::vec::Vec::new();
                let _args_self = ();
                let _args_tail = ();
            });
        }
        else {
            output.push(quote! { let mut _args_handles = ::embers_transpile::__private::std::vec::Vec::new(); });

            let mut args_tail = vec![];

            for (i, expr) in args.iter().enumerate() {
                let arg_name = name_gen.tmp_var("arg");
                output.push(quote! {
                    let #arg_name = ::embers_transpile::__private::AsExpression::as_expression(&#expr, &mut _function_builder)?;
                    if let Some(arg) = #arg_name.get_handle() {
                        _args_handles.push(arg);
                    }
                });
                if i == 0 {
                    output.push(quote! {
                        //let _args_self = #private::PhantomReceiver::from(#arg_name);
                        let _args_self = ::embers_transpile::__private::std::convert::Into::into(#arg_name);
                    })
                }
                else {
                    args_tail.push(arg_name);
                }
            }

            output.push(quote! {
                let _args_tail = (#(#args_tail),*);
            });
        }

        output.push(quote! {
            let #out = ::embers_transpile::__private::Callable::call(#func_ref, _args_self, _args_tail, _args_handles, &mut _function_builder)?;
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
        // fixme
        let out = name_gen.tmp_var("callres");

        output.push(quote! {
            let _arg_handles = [#receiver.get_handle(), #(#args.get_handle()),*].into_iter().flatten().collect();
            let _gen = ::embers_transpile::__private::PhantomReceiver::from(#receiver).#method((#(#args),*));
            let #out = _function_builder.add_call(&_func, _gen, _arg_handles)?;
        });

        ExprOut::from(out)
    }

    macro_rules! emit_func_call_std {
        ($func_path:path, $($args:expr),*) => {{
            let args = &[$($args),*];
            emit_func_call(output, name_gen, quote!{ &::embers_transpile::__private::shader_std::$func_path }, args)
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
            if let Some(block_result) = block_result {
                block_output.push(block_result);
            }
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

            path.path.clone().into()
        }
        Expr::Reference(ref_) => {
            todo!("ref")
        }
        Expr::Repeat(repeat) => todo!("process_expr -> Expr::Repeat"),
        Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                let out = process_expr(expr, output, name_gen)?;
                output.push(quote! {
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
                if let Some(r) = &_return_value {
                    _function_builder.add_emit(r)?;
                }
                _function_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
                    value: _return_value,
                });
                let #out = ::embers_transpile::__private::ExpressionHandle::<()>::from_phantom();
            });
            out.into()
        }
        Expr::Struct(strct) => {
            let ty = &strct.path;

            output.push(quote! {
                let _struct_ty = _function_builder.module_builder.get_type_by_id_or_add_it::<#ty>().get_type().expect("type is not a struct");
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
            let arg = process_expr(&unary.expr, output, name_gen)?;
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
