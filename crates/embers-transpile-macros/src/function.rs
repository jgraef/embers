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
use syn::{
    Attribute,
    Block,
    FnArg,
    Ident,
    ImplItemFn,
    ItemFn,
    Macro,
    Pat,
    Path,
    ReturnType,
    Signature,
    Stmt,
    Type,
    Visibility,
};

use crate::{
    error::Error,
    expression::ExprOut,
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
        let body = generate_function_body_inline(ret, block)?;
        quote! {
            #vis #sig_transformed {
                #unpack_receiver
                ::embers_transpile::__private::InlineCallGenerator::new(
                    move |mut _block_builder: &mut ::embers_transpile::__private::BlockBuilder| {
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

                //let ty = &receiver.ty;
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

                //let ty = &receiver.ty;
                //let ty = if receiver.reference.is_some() {
                //    // todo: what address space do we use? we could abuse lifetimes for this.
                //    quote! { ::embers_transpile::__private::Pointer<Self,
                // ::embers_transpile::__private::address_space::Private> }
                //}
                //else {
                //    quote! { #ty }
                //};

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
                                            let #ident = ::embers_transpile::__private::ExpressionHandle::<#ty>::empty();
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
        // apparently we must not emit this expression
        //_block_builder.add_emit(&#result_var)?;
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
                //let ty = &pat_type.ty;

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

fn generate_function_body_inline(ret: TokenStream, block: &Block) -> Result<TokenStream, Error> {
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

pub fn implicit_unit(
    expr: Option<ExprOut>,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> ExprOut {
    expr.unwrap_or_else(|| {
        let var = name_gen.tmp_var("implicit_unit");
        output.push(quote! {
            let #var = ::embers_transpile::__private::ExpressionHandle::<::embers_transpile::__private::Unit>::empty();
        });
        ExprOut::from(var)
    })
}

pub fn process_block_inner(
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
                let rhs = crate::expression::process_expr(&rhs.expr, output, name_gen)?;
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
        Stmt::Item(item) => {
            let item_output = crate::item::process_item(item, None)?;
            output.push(item_output);
            None
        }
        Stmt::Expr(expr, semi) => {
            let out = crate::expression::process_expr(expr, output, name_gen)?;
            semi.is_none().then_some(out)
        }
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

pub fn process_macro(
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
