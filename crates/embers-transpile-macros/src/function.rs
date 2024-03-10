use std::sync::mpsc::Receiver;

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
pub enum BuiltinType {
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

impl ToTokens for AddressSpace {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let out = match self {
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
        tokens.extend(out);
    }
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
pub enum EntryPointArgMeta {
    Builtin(BuiltinType),
    Global(GlobalMeta),
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(transpile))]
struct EntrypointArgAttributes {
    #[darling(flatten)]
    meta: EntryPointArgMeta,
}

pub fn process_bare_function(
    input: &ItemFn,
    attributes: Option<&[NestedMeta]>,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let args = FnMeta::parse(attributes, &input.attrs)?;
    process_function(&input.vis, &input.sig, &input.block, &args, name_gen)
}

pub fn process_impl_function(
    input: &ImplItemFn,
    attributes: Option<&[NestedMeta]>,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let args = FnMeta::parse(attributes, &input.attrs)?;
    process_function(&input.vis, &input.sig, &input.block, &args, name_gen)
}

fn process_function(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
    attrs: &FnMeta,
    name_gen: &mut NameGen,
) -> Result<TokenStream, Error> {
    let SignatureTransform {
        new_sig,
        inputs,
        output_type,
        ident,
        ..
    } = SignatureTransform::new(sig, name_gen, attrs.entrypoint)?;

    let func_name_lit = ident_to_literal(&ident);

    let mut entrypoint_create_args = TokenBuffer::default();
    let mut entrypoint_add_globals = TokenBuffer::default();
    let mut collect_args = TokenBuffer::default();
    let mut bind_args = TokenBuffer::default();
    let mut args_as_expression_handles = TokenBuffer::default();
    let mut arg_index: usize = 0;
    let mut rebind_self = None;

    for input in &inputs {
        let name_expr = input.name_expression();
        let name = input.ident();
        let ty = input.ty();
        let mut global_meta = None;
        let mut binding_expr = None;

        if let Input::Receiver { .. } = input {
            rebind_self = Some(quote! {
                let #name = ::embers_transpile::__private::MaybeSelf::into_argument(self);
            });
        }

        match input.entry_point_attrs() {
            Some(EntryPointArgMeta::Global(global)) => {
                global_meta = Some(global);
            }
            Some(EntryPointArgMeta::Builtin(builtin)) => {
                binding_expr = Some(quote! {
                    ::embers_transpile::__private::naga::Binding::BuiltIn(#builtin)
                });
            }
            None => {}
        }

        if attrs.entrypoint {
            // we don't actually have an function argument, so we just make one

            // fixme: if it's a global it might be a pointer type
            entrypoint_create_args.push(quote! {
                let #name = <::embers_transpile::__private::Argument::<#ty> as ::embers_transpile::__private::std::default::Default>::default();
            });
        }

        // generates an expression with type (ExpressionHandle<Arg1>,
        // ExpressionHandle<Arg2>, ...)
        args_as_expression_handles.push(quote! {
            #name.as_empty_expression_handle(),
        });

        if let Some(global) = global_meta {
            if !input.is_wild() {
                let global_ident = name_gen.tmp_var("global");

                let binding_expr = if let Some(binding) = global.binding {
                    let group = global.group.unwrap_or_default();
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

                let address_space = &global.address_space;

                entrypoint_add_globals.push(quote!{
                    let #global_ident = module_builder.get_global_variable_or_add_it::<#ty, #address_space>(
                        #name_expr.expect("global variables must be named"),
                        #binding_expr,
                    )?;
                });

                bind_args.push(quote!{
                    let #name = ::embers_transpile::__private::ExpressionHandle::from_global_arg(#name, #global_ident, _block_builder)?;
                });
            }
        }
        else {
            let binding_expr = binding_expr
                .map(|expr| {
                    quote! {
                        ::embers_transpile::__private::Some(#expr)
                    }
                })
                .unwrap_or_else(|| quote! {::embers_transpile::__private::None});

            collect_args.push(quote! {
                _args.push(_function_builder.add_input(#name_expr, #name, false, #binding_expr)?.as_dyn());
            });

            // todo: if the arg is mut, we need to create a variable for it.
            bind_args.push(quote!{
                let #name = ::embers_transpile::__private::ExpressionHandle::from_arg(#name, _args[#arg_index], _block_builder)?;
            });

            arg_index += 1;
        }
    }

    // generate function body
    let mut body = TokenBuffer::default();
    let output_expr = process_block(block, &mut body, name_gen)?;

    // generate the expression that returns a Return<Output> (the function
    // generator)
    let create_function = quote! {
        #rebind_self
        ::embers_transpile::__private::return_function_with_closure(
            (#args_as_expression_handles), // we pass this so the return_function_with_closure can infer the type for Args
            move |mut _block_builder, _args| {
                #bind_args
                #body
                let _output = ::embers_transpile::__private::AsExpression::as_expression(&#output_expr, _block_builder)?;
                Ok(_output.as_dyn())
            },
            move |_function_builder, _body_generator| {
                _function_builder.add_name(#func_name_lit);
                let mut _args = ::embers_transpile::__private::Vec::new();
                #collect_args
                _function_builder.add_output::<#output_type>()?;
                let mut _block_builder = _function_builder.block();
                _body_generator(&mut _block_builder, _args)?;
                _block_builder.finish_root();
                Ok(())
            }
        )
    };

    let output = if attrs.entrypoint {
        let generics = &sig.generics;
        quote! {
            #vis fn #ident #generics () -> ::embers_transpile::__private::Result<::embers_transpile::__private::Module, ::embers_transpile::__private::BuilderError> {
                #entrypoint_create_args
                let mut module_builder = ::embers_transpile::__private::ModuleBuilder::default();
                #entrypoint_add_globals
                let function: ::embers_transpile::__private::Return<::embers_transpile::__private::Unit> = #create_function;
                module_builder.add_entrypoint(
                    function.generator,
                )?;
                let module = module_builder.build();
                Ok(module)
            }
        }
    }
    else {
        quote! {
            #vis #new_sig {
                #create_function
            }
        }
    };

    Ok(output)
}

pub enum Input {
    Receiver {
        pointer: bool,
    },
    Plain {
        name: Ident,
        ty: TokenStream,
        entry_point_attrs: Option<EntryPointArgMeta>,
    },
    Wild {
        assigned_name: Ident,
        ty: TokenStream,
        entry_point_attrs: Option<EntryPointArgMeta>,
    },
}

impl Input {
    pub fn entry_point_attrs(&self) -> Option<&EntryPointArgMeta> {
        match self {
            Input::Receiver { .. } => None,
            Input::Plain {
                entry_point_attrs, ..
            }
            | Input::Wild {
                entry_point_attrs, ..
            } => entry_point_attrs.as_ref(),
        }
    }

    pub fn name_expression(&self) -> TokenStream {
        match self {
            Input::Receiver { .. } => {
                quote! {::embers_transpile::__private::Some("self".to_owned())}
            }
            Input::Plain { name, .. } => {
                let name_lit = ident_to_literal(name);
                quote! {::embers_transpile::__private::Some(#name_lit.to_owned())}
            }
            Input::Wild { .. } => quote! {::embers_transpile::__private::None},
        }
    }

    pub fn ident(&self) -> Ident {
        match self {
            Input::Receiver { .. } => Ident::new("_self", Span::call_site()),
            Input::Plain { name, .. } => name.clone(),
            Input::Wild { assigned_name, .. } => assigned_name.clone(),
        }
    }

    pub fn ty(&self) -> TokenStream {
        match self {
            Input::Receiver { pointer } => {
                if *pointer {
                    quote! { ::embers_transpile::__private::SelfPointerArgument<Self, impl ::embers_transpile::__private::AddressSpace> }
                }
                else {
                    quote! { ::embers_transpile::__private::SelfArgument<Self> }
                }
            }
            Input::Plain { ty, .. } | Input::Wild { ty, .. } => quote! { #ty },
        }
    }

    pub fn is_wild(&self) -> bool {
        match self {
            Input::Wild { .. } => true,
            _ => false,
        }
    }
}

pub struct SignatureTransform {
    pub new_sig: TokenStream,
    pub has_receiver: bool,
    pub inputs: Vec<Input>,
    pub output_type: TokenStream,
    pub ident: Ident,
}

impl SignatureTransform {
    pub fn new(sig: &Signature, name_gen: &mut NameGen, entry_point: bool) -> Result<Self, Error> {
        let mut has_receiver = false;
        let mut input_sig = vec![];
        let mut inputs = vec![];

        let ident = &sig.ident;

        for input in &sig.inputs {
            match input {
                FnArg::Receiver(receiver) => {
                    assert!(!entry_point);
                    assert!(!has_receiver);
                    has_receiver = true;

                    let ty = if receiver.reference.is_some() {
                        quote! { ::embers_transpile::__private::SelfPointerArgument<Self, impl ::embers_transpile::__private::AddressSpace> }
                    }
                    else {
                        quote! { ::embers_transpile::__private::SelfArgument<Self> }
                    };
                    input_sig.push(quote! { self: #ty });

                    inputs.push(Input::Receiver {
                        pointer: receiver.reference.is_some(),
                    });
                }
                FnArg::Typed(pat_type) => {
                    let pat = &pat_type.pat;
                    let mut ty = pat_type.ty.clone();
                    map_types(&mut ty, TypePosition::Argument);

                    let entry_point_attrs = entry_point
                        .then(|| {
                            EntrypointArgAttributes::from_attributes(&pat_type.attrs)
                                .map(|a| a.meta)
                        })
                        .transpose()?;

                    match &**pat {
                        Pat::Ident(pat_ident) => {
                            input_sig.push(
                                quote! { #pat: ::embers_transpile::__private::Argument<#ty> },
                            );
                            inputs.push(Input::Plain {
                                name: pat_ident.ident.clone(),
                                ty: quote! {#ty},
                                entry_point_attrs,
                            });
                        }
                        Pat::Wild(_) => {
                            let assigned_name = name_gen.tmp_var("wild");
                            input_sig.push(quote! { #assigned_name: ::embers_transpile::__private::Argument<#ty> });
                            inputs.push(Input::Wild {
                                assigned_name,
                                ty: quote! {#ty},
                                entry_point_attrs,
                            });
                        }
                        _ => panic!("unsupported pattern in argument position"),
                    }
                }
            }
        }

        let output_type = match &sig.output {
            ReturnType::Default => quote! { ::embers_transpile::__private::Unit },
            ReturnType::Type(_, ty) => {
                let mut ty = ty.clone();
                map_types(&mut ty, TypePosition::ReturnType);
                quote! { #ty }
            }
        };

        let generics = &sig.generics;

        let new_sig = quote! {
            fn #ident #generics (#(#input_sig),*) -> ::embers_transpile::__private::Return<#output_type>
        };

        Ok(Self {
            new_sig,
            has_receiver,
            inputs,
            output_type,
            ident: ident.clone(),
        })
    }
}

/*
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
            value: #result_var.get_naga(),
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
 */

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

pub fn process_block(
    input: &Block,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    let mut result = None;

    let mut block = TokenBuffer::default();

    for stmt in &input.stmts {
        result = process_stmt(stmt, &mut block, name_gen)?;
    }

    // return output from last statement
    if let Some(result) = result {
        block.push(result);
    }
    else {
        block.push(quote!{ ::embers_transpile::__private::ExpressionHandle::<::embers_transpile::__private::Unit>::empty() })
    }

    let var = name_gen.tmp_var("block");
    output.push(quote! {
        let #var = {
            #block
        };
    });

    Ok(var.into())
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
                             let #ident = ::embers_transpile::__private::LetBinding::<#lhs_ty>::from_expr(#rhs, _block_builder.function_builder.scope_id());
                        });
                    }
                    else {
                        output.push(
                            quote! { let #ident = ::embers_transpile::__private::LetBinding::<#lhs_ty>::unbound(_block_builder.function_builder.scope_id()); },
                        );
                    }
                }
            }

            None
        }
        Stmt::Item(item) => {
            let item_output = crate::item::process_item(item, None, name_gen)?;
            output.push(item_output);
            None
        }
        Stmt::Expr(expr, semi) => {
            let out = crate::expression::process_expr(expr, output, name_gen)?;
            semi.is_none().then_some(out)
        }
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
