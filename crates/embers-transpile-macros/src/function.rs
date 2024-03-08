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
    Attribute,
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
    Token,
    Type,
    UnOp,
    Visibility,
};

use crate::{
    error::Error,
    utils::{
        ident_to_literal,
        map_types,
        NameGen,
        TokenBuffer,
        TypePosition,
    },
};

#[derive(Debug, FromAttributes)]
#[darling(attributes(transpile))]
pub struct FnAttributes {
    #[darling(flatten)]
    pub meta: FnMeta,
}

#[derive(Debug, FromMeta)]
pub struct FnMeta {
    #[darling(default)]
    pub entrypoint: bool,
    #[darling(default)]
    pub inline: bool,
}

impl FnMeta {
    pub fn parse(
        nested_meta: Option<&[NestedMeta]>,
        item_attributes: &[Attribute],
    ) -> Result<Self, Error> {
        let meta = if let Some(nested_meta) = nested_meta {
            FnMeta::from_list(nested_meta)?
        }
        else {
            FnAttributes::from_attributes(item_attributes)?.meta
        };
        Ok(meta)
    }
}

#[derive(Debug, FromMeta)]
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
                quote! { ::embers_transpile::__private::naga::BuiltIn::Position { invariant: #invariant } }
            }
            BuiltinType::ViewIndex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::ViewIndex }
            }
            BuiltinType::BaseInstance => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::BaseInstance }
            }
            BuiltinType::BaseVertex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::BaseVertex }
            }
            BuiltinType::ClipDistance => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::ClipDistance }
            }
            BuiltinType::CullDistance => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::CullDistance }
            }
            BuiltinType::InstanceIndex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::InstanceIndex }
            }
            BuiltinType::PointSize => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::PointSize }
            }
            BuiltinType::VertexIndex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::VertexIndex }
            }
            BuiltinType::FragDepth => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::FragDepth }
            }
            BuiltinType::PointCoord => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::PointCoord }
            }
            BuiltinType::FrontFacing => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::FrontFacing }
            }
            BuiltinType::PrimitiveIndex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::PrimitiveIndex }
            }
            BuiltinType::SampleIndex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::SampleIndex }
            }
            BuiltinType::SampleMask => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::SampleMask }
            }
            BuiltinType::GlobalInvocationId => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::GlobalInvocationId }
            }
            BuiltinType::LocalInvocationId => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::LocalInvocationId }
            }
            BuiltinType::LocalInvocationIndex => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::LocalInvocationIndex }
            }
            BuiltinType::WorkGroupId => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::WorkGroupId }
            }
            BuiltinType::WorkGroupSize => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::WorkGroupSize }
            }
            BuiltinType::NumWorkGroups => {
                quote! { ::embers_transpile::__private::naga::BuiltIn::NumWorkGroups }
            }
        };
        tokens.extend(out);
    }
}

#[derive(Clone, Copy, Debug, FromMeta)]
pub enum StorageAccess {
    #[darling(word)]
    Read,
    Write,
}

#[derive(Clone, Copy, Debug, FromMeta)]
pub enum AddressSpace {
    Function,
    Private,
    WorkGroup,
    Uniform,
    Storage(StorageAccess),
    Handle,
    PushConstant,
}

impl Default for AddressSpace {
    fn default() -> Self {
        Self::Private
    }
}

#[derive(Debug, Default, FromMeta)]
pub struct GlobalMeta {
    pub group: Option<u32>,
    pub binding: Option<u32>,
    #[darling(default)]
    pub address_space: AddressSpace,
}

#[derive(Debug, FromMeta)]
enum EntryPointArgMeta {
    Builtin(BuiltinType),
    Global(GlobalMeta),
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(transpile))]
struct EntrypointArgAttributes {
    #[darling(flatten)]
    meta: EntryPointArgMeta,
}

pub fn process_entrypoint(input: &ItemFn, _args: &FnMeta) -> Result<TokenStream, Error> {
    let vis = &input.vis;
    let sig = &input.sig;
    let generics = &sig.generics;
    let block = &input.block;
    let ident = &input.sig.ident;
    let ret = quote! { ::embers_transpile::__private::Unit };

    let body = generate_function_body(sig, ret, block, true)?;

    let generated = quote! {
        #vis fn #ident #generics () -> ::embers_transpile::__private::Result<::embers_transpile::__private::Module, ::embers_transpile::__private::BuilderError> {
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

pub fn process_bare_function(
    input: &ItemFn,
    attributes: Option<&[NestedMeta]>,
) -> Result<TokenStream, Error> {
    let args = FnMeta::parse(attributes, &input.attrs)?;
    if args.entrypoint {
        assert!(!args.inline);
        process_entrypoint(input, &args)
    }
    else {
        process_function(&input.vis, &input.sig, &input.block, &args)
    }
}

pub fn process_impl_function(
    input: &ImplItemFn,
    attributes: Option<&[NestedMeta]>,
) -> Result<TokenStream, Error> {
    let args = FnMeta::parse(attributes, &input.attrs)?;
    process_function(&input.vis, &input.sig, &input.block, &args)
}

fn process_function(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
    args: &FnMeta,
) -> Result<TokenStream, Error> {
    let (sig_transformed, ret, has_receiver) = transform_signature_to_generator(sig);

    let unpack_receiver = if has_receiver {
        quote! { let _self = self.unpack(); }
    }
    else {
        quote! {}
    };

    let generated = if args.inline {
        let body = generate_function_body_inline(sig, ret, block)?;
        quote! {
            #vis #sig_transformed {
                #unpack_receiver
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
                #unpack_receiver
                ::embers_transpile::__private::CallGenerator::new(
                    move |mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder| {
                        #body
                    },
                    move |
                        mut _block_builder: &mut ::embers_transpile::__private::BlockBuilder,
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

pub fn transform_signature_to_generator(sig: &Signature) -> (TokenStream, TokenStream, bool) {
    let mut has_receiver = false;
    let mut inputs = vec![];

    let ident = &sig.ident;

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                assert!(!has_receiver);
                has_receiver = true;

                let ty = &receiver.ty;
                let ty = if receiver.reference.is_some() {
                    // todo: what address space?
                    quote! { ::embers_transpile::__private::PhantomReceiverPointer<Self, impl ::embers_transpile::__private::AddressSpace> }
                }
                else {
                    quote! { ::embers_transpile::__private::PhantomReceiver<Self> }
                };

                inputs.push(quote! { self: #ty });
            }
            FnArg::Typed(pat_type) => {
                let pat = &pat_type.pat;
                let mut ty = pat_type.ty.clone();
                map_types(&mut ty, TypePosition::Argument);

                // todo: accept impl AsExpressionHandle
                inputs.push(quote! { #pat: ::embers_transpile::__private::ExpressionHandle<#ty> });
            }
        }
    }

    let ret = match &sig.output {
        ReturnType::Default => quote! { ::embers_transpile::__private::Unit },
        ReturnType::Type(_, ty) => {
            let mut ty = ty.clone();
            map_types(&mut ty, TypePosition::Argument);
            quote! { #ty }
        }
    };

    let generics = &sig.generics;

    let sig = quote! {
        fn #ident #generics (#(#inputs),*) -> impl ::embers_transpile::__private::GenerateCall<Return = #ret>
    };

    (sig, ret, has_receiver)
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
                    quote! { ::embers_transpile::__private::Pointer<Self, ::embers_transpile::__private::address_space::Private> }
                }
                else {
                    quote! { #ty }
                };

                body.push(quote! {
                    let _self = _function_builder.add_input_receiver(_self)?;
                });
            }
            FnArg::Typed(pat_type) => {
                let mut ty = pat_type.ty.clone();
                map_types(&mut ty, TypePosition::Argument);

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let ident = &pat_ident.ident;
                        let name = ident_to_literal(ident);
                        let is_mut = pat_ident.mutability.is_some();

                        let mut is_global = false;
                        if entrypoint {
                            let attrs = EntrypointArgAttributes::from_attributes(&pat_type.attrs)
                                .map_err(|e| {
                                println!("expected error: {e}");
                                e
                            })?;

                            match attrs.meta {
                                EntryPointArgMeta::Builtin(builtin) => {
                                    body.push(
                                        quote! {
                                            let _binding = Some(::embers_transpile::__private::naga::Binding::BuiltIn(#builtin));
                                            // this handle is only used by add_input_named to infer the type of the argument
                                            let #ident = ::embers_transpile::__private::ExpressionHandle::<#ty>::from_empty();
                                        },
                                    );
                                }
                                EntryPointArgMeta::Global(meta) => {
                                    is_global = true;

                                    let address_space = match meta.address_space {
                                        AddressSpace::Function => {
                                            quote! { ::embers_transpile::__private::address_space::Function }
                                        }
                                        AddressSpace::Private => {
                                            quote! { ::embers_transpile::__private::address_space::Private }
                                        }
                                        AddressSpace::WorkGroup => {
                                            quote! { ::embers_transpile::__private::address_space::WorkGroup }
                                        }
                                        AddressSpace::Uniform => {
                                            quote! { ::embers_transpile::__private::address_space::Uniform }
                                        }
                                        AddressSpace::Storage(access) => {
                                            match access {
                                                StorageAccess::Read => {
                                                    quote! { ::embers_transpile::__private::address_space::StorageRead }
                                                }
                                                StorageAccess::Write => {
                                                    quote! { ::embers_transpile::__private::address_space::StorageReadWrite }
                                                }
                                            }
                                        }
                                        AddressSpace::Handle => {
                                            quote! { ::embers_transpile::__private::AddressSpace::Handle }
                                        }
                                        AddressSpace::PushConstant => {
                                            quote! { ::embers_transpile::__private::AddressSpace::PushConstant }
                                        }
                                    };

                                    let binding_expr = if let Some(binding) = meta.binding {
                                        let group = meta.group.unwrap_or_default();
                                        quote! {
                                            ::embers_transpile::__private::Some(::embers_transpile::__private::naga::ResourceBinding {
                                                group: #group,
                                                binding: #binding,
                                            })
                                        }
                                    }
                                    else {
                                        quote! { ::embers_transpile::__private::None }
                                    };

                                    body.push(quote!{
                                        let #ident = _function_builder.module_builder.get_global_variable_or_add_it::<#ty, #address_space>(
                                            #name,
                                            #binding_expr,
                                        )?;
                                    })
                                }
                            }
                        }
                        else {
                            body.push(quote! { let _binding = None; });
                        }

                        if !is_global {
                            body.push(quote! {
                                let #ident = _function_builder.add_input_named(#name, #ident, #is_mut, _binding)?;
                            });
                        }
                    }
                    syn::Pat::Wild(_) => {
                        // pretty sure we don't have to do anything here
                        //body.push(quote! {
                        //    _function_builder.add_input_wild::<#ty>()?;
                        //});
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

    body.push(quote! {
        let mut _block_builder = _function_builder.block();
    });

    let result_var = process_block_inner(block, &mut body, &mut name_gen)?;

    body.push(quote! {
        // make sure the return value type matches
        let #result_var = ::embers_transpile::__private::AsExpression::as_expression(&#result_var, &mut _block_builder)?;
        let #result_var: ::embers_transpile::__private::ExpressionHandle<#ret> = #result_var;

        // return
        _block_builder.add_emit(&#result_var)?;
        _block_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
            value: #result_var.get_handle(),
        })?;

        _block_builder.finish_root();
        ::embers_transpile::__private::Ok::<(), ::embers_transpile::__private::BuilderError>(())
    });

    Ok(body.into_token_stream())
}

fn generate_function_call(sig: &Signature) -> Result<TokenStream, Error> {
    let mut arg_names = vec![];

    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                assert!(receiver.colon_token.is_none());
                arg_names.push(quote! { self });
            }
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let var = &pat_ident.ident;
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

    let num_args = arg_names.len();

    Ok(quote! {
        {
            let _args: ::embers_transpile::__private::std::iter::Flatten<
                ::embers_transpile::__private::std::array::IntoIter<
                    ::embers_transpile::__private::Option<
                        ::embers_transpile::__private::naga::Handle<
                            ::embers_transpile::__private::naga::Expression
                        >
                    >,
                    #num_args
                >
            > = [
                #(::embers_transpile::__private::AsExpression::as_expression(&#arg_names, &mut _block_builder)?.get_handle()),*
            ].into_iter().flatten();
            _block_builder.add_call(
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

    // the inputs already have the right names and will be moved into to generating
    // closure

    let result_var = process_block_inner(block, &mut body, &mut name_gen)?;

    // make sure the return value type matches
    body.push(quote! {
        let #result_var = ::embers_transpile::__private::AsExpression::as_expression(&#result_var, &mut _block_builder)?;
        let #result_var: ::embers_transpile::__private::ExpressionHandle<#ret> = #result_var;
        // should we emit here?
        //_function_builder.add_emit(&#result_var)?;
        ::embers_transpile::__private::Ok::<_, ::embers_transpile::__private::BuilderError>(#result_var)
    });

    Ok(quote! {
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
            let #var = ::embers_transpile::__private::ExpressionHandle::<::embers_transpile::__private::Unit>::from_empty();
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
                let rhs = process_expr(&rhs.expr, output, name_gen)?;
                output.push(quote!{
                    let #rhs = ::embers_transpile::__private::AsExpression::as_expression(&#rhs, &mut _block_builder)?;
                });
                Some(rhs)
            }
            else {
                None
            };

            let lhs = LetLhs::from_pat(&local.pat);

            let lhs_ty = if let Some(ty) = lhs.ty {
                let mut ty = ty.clone();
                map_types(&mut ty, TypePosition::Let);
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
                        let #ident = _block_builder.function_builder.add_local_variable::<#lhs_ty>(#ident_literal, #init)?;
                    });
                }
                else {
                    if let Some(rhs) = &rhs {
                        output.push(quote! {
                            _block_builder.function_builder.name_expression(#ident_literal, #rhs.clone())?;
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
                let mut block_builder = &mut _block_builder;

                #tokens
            };
        });
    }
    else if path.is_ident("todo") {
        // just put this inplace, so we panic during code generation
        output.push(quote! {
            #macro_;
            let #var = ::embers_transpile::__private::ExpressionHandle::<::embers_transpile::__private::Unit>::from_phantom();
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
                let #base = ::embers_transpile::__private::AsExpression::as_expression(&#base, &mut _block_builder)?;
            });

            let field_accessor = field_accessor_for_member(&field.member);

            let out = name_gen.tmp_var("field");
            output.push(quote! {
                let #out = ::embers_transpile::__private::FieldAccess::<::embers_transpile::__private::#field_accessor>::access(&mut _block_builder, #base)?;
            });
            out.into()
        }
        Expr::ForLoop(for_) => todo!("process_expr -> Expr::ForLoop"),
        Expr::If(if_) => todo!("process_expr -> Expr::If"),
        Expr::Index(index) => todo!("process_expr -> Expr::Index"),
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
                let #out = ::embers_transpile::__private::AsPointer::as_pointer(&#expr, _block_builder.function_builder)?;
            });
            out.into()
        }
        Expr::Repeat(repeat) => todo!("process_expr -> Expr::Repeat"),
        Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                let out = process_expr(expr, output, name_gen)?;
                output.push(quote! {
                    let #out = ::embers_transpile::__private::AsExpression::as_expression(&#out, &mut _block_builder)?;
                    _block_builder.function_builder.add_emit(&#out)?;
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
                _block_builder.function_builder.add_statement(::embers_transpile::__private::naga::Statement::Return {
                    value: _return_value,
                })?;
                let #out = ::embers_transpile::__private::ExpressionHandle::<::embers_transpile::__private::Unit>::from_empty();
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
        Expr::Try(try_) => todo!("process_expr -> Expr::Try"),
        Expr::Tuple(tuple) => todo!("process_expr -> Expr::Tuple"),
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
        Expr::While(while_) => todo!("process_expr -> Expr::While"),
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
