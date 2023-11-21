use std::{fmt::Display, str::FromStr};

use darling::{FromMeta, FromAttributes, ast::NestedMeta, util::parse_attribute_to_meta_list, FromMetaItem};
use proc_macro2::{
    Literal,
    Span,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use strum::EnumString;
use syn::{
    spanned::Spanned,
    Block,
    DataStruct,
    Expr,
    Fields,
    FnArg,
    Ident,
    ItemFn,
    ItemImpl,
    ItemTrait,
    Pat,
    Path,
    ReturnType,
    Stmt,
    UnOp, Attribute, Meta,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("syn error")]
    Syn(#[from] syn::Error),
}

#[derive(Default)]
struct TokenBuffer {
    buf: TokenStream,
}

impl TokenBuffer {
    pub fn push(&mut self, tokens: impl ToTokens) {
        tokens.to_tokens(&mut self.buf);
    }
}

impl ToTokens for TokenBuffer {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let buf = &self.buf;
        tokens.extend(quote! { #buf });
    }
}

#[derive(Debug, Default)]
struct NameGen {
    next_id: usize,
}

impl NameGen {
    pub fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn tmp_var(&mut self, prefix: impl Display) -> Ident {
        let name = format!("_tmp_{prefix}_{}", self.next_id());
        Ident::new(&name, Span::call_site())
    }

    pub fn var(&mut self, name: impl Display, span: Span) -> Ident {
        Ident::new(&format!("var_{name}"), span)
    }
}

pub fn impl_ricsl_type_for_struct(ident: &Ident, strct: &DataStruct) -> Result<TokenStream, Error> {
    // todo: how do we make the type name unique????
    // todo: handle generics

    let ident_literal = ident_to_literal(&ident);

    let mut struct_fields = vec![];
    let mut phantom_eval = vec![];

    match &strct.fields {
        Fields::Unit => {}
        Fields::Named(named) => {
            for field in &named.named {
                let field_name_literal = ident_to_literal(field.ident.as_ref().unwrap());
                let field_type = &field.ty;
                struct_fields.push(
                    quote! { struct_builder.add_named_field::<#field_type>(#field_name_literal); },
                );
                phantom_eval.push(
                    quote! { && <#field_type as ::embers_ricsl::__private::RicslType>::PHANTOM }
                );
            }
        }
        Fields::Unnamed(unnamed) => {
            for field in &unnamed.unnamed {
                let field_type = &field.ty;
                struct_fields.push(quote! { struct_builder.add_unnamed_field::<#field_type>(); });
            }
        }
    }

    let generated = quote! {
        impl ::embers_ricsl::__private::RicslType for #ident {
            const PHANTOM: bool = {
                true #(#phantom_eval)*
            };

            fn add_to_module(module_builder: &mut ::embers_ricsl::__private::ModuleBuilder) -> ::embers_ricsl::__private::Handle<::embers_ricsl::__private::Type> {
                let mut struct_builder = module_builder.add_struct::<Self>(#ident_literal);
                #(#struct_fields)*
                struct_builder.build()
            }
        }
    };

    Ok(generated)
}

pub fn impl_ricsl_trait(input: &ItemTrait) -> Result<TokenStream, Error> {
    let ident = &input.ident;
    let ident_literal = ident_to_literal(&ident);

    todo!();
}

pub fn impl_ricsl_impl(input: &ItemImpl) -> Result<TokenStream, Error> {
    todo!();
}

pub fn process_function(input: &ItemFn) -> Result<TokenStream, Error> {
    let vis = &input.vis;

    let mut output = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    let ident = &input.sig.ident;
    let ident_literal = ident_to_literal(&ident);

    output.push(quote! {
        let mut function_builder = module_builder.add_function(#ident_literal);
    });

    let mut args = vec![];

    for input in &input.sig.inputs {
        match input {
            FnArg::Receiver(_) => {
                output.push(quote! { function_builder.add_input_receiver(); });
                args.push(quote! { self: ::embers_ricsl::__private::ExpressionHandle<Self> });
            }
            FnArg::Typed(pat_type) => {
                let ty = &pat_type.ty;

                match &*pat_type.pat {
                    syn::Pat::Ident(pat_ident) => {
                        assert!(pat_ident.by_ref.is_none());
                        assert!(pat_ident.subpat.is_none());

                        let name = ident_to_literal(&pat_ident.ident);
                        let is_mut = pat_ident.mutability.is_some();

                        // todo: parse attributes for bindings

                        let var = name_gen.var(pat_ident.ident.to_string(), pat_ident.span());
                        output.push(quote! {
                            let #var = function_builder.add_input_named::<#ty>(#name, #is_mut, None);
                        });

                        args.push(quote!{ #var: ::embers_ricsl::__private::ExpressionHandle<#ty> });
                    }
                    syn::Pat::Wild(_) => {
                        output.push(quote! {
                            function_builder.add_input_wild::<#ty>();
                        });
                    }
                    _ => panic!("unsupported"),
                }
            }
        }
    }

    let ret = match &input.sig.output {
        ReturnType::Default => quote!{ ::embers_ricsl::__private::ExpressionHandle<()> },
        ReturnType::Type(_, ty) => {
            output.push(quote! {
                function_builder.add_output::<#ty>();
            });
            quote!{ ::embers_ricsl::__private::ExpressionHandle<#ty> }
        }
    };

    let block = process_block_inner(&input.block, &mut output, &mut name_gen)?;

    output.push(quote! { function_builder.build() });

    let generated = quote! {
        #vis fn #ident(#(#args),*) -> impl ::embers_ricsl::__private::FunctionGenerator<Return = #ret> {
            struct Gen;

            impl ::embers_ricsl::__private::FunctionGenerator for Gen {
                type Return = #ret;

                fn generate(&self, module_builder: &mut ::embers_ricsl::__private::ModuleBuilder) -> ::embers_ricsl::__private::Handle<::embers_ricsl::__private::Type> {
                    #output
                }
            }

            Gen
        }
    };

    Ok(generated)
}


#[derive(Debug, EnumString)]
#[strum(serialize_all="snake_case")]
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
            BuiltinType::Position { invariant } => quote! { ::embers_ricsl::__private::naga::BuiltIn::Position { #invariant } },
            BuiltinType::ViewIndex => quote! { ::embers_ricsl::__private::naga::BuiltIn::ViewIndex },
            BuiltinType::BaseInstance => quote! { ::embers_ricsl::__private::naga::BuiltIn::BaseInstance },
            BuiltinType::BaseVertex => quote! { ::embers_ricsl::__private::naga::BuiltIn::BaseVertex },
            BuiltinType::ClipDistance => quote! { ::embers_ricsl::__private::naga::BuiltIn::ClipDistance },
            BuiltinType::CullDistance => quote! { ::embers_ricsl::__private::naga::BuiltIn::CullDistance },
            BuiltinType::InstanceIndex => quote! { ::embers_ricsl::__private::naga::BuiltIn::InstanceIndex },
            BuiltinType::PointSize => quote! { ::embers_ricsl::__private::naga::BuiltIn::PointSize },
            BuiltinType::VertexIndex => quote! { ::embers_ricsl::__private::naga::BuiltIn::VertexIndex },
            BuiltinType::FragDepth => quote! { ::embers_ricsl::__private::naga::BuiltIn::FragDepth },
            BuiltinType::PointCoord => quote! { ::embers_ricsl::__private::naga::BuiltIn::PointCoord },
            BuiltinType::FrontFacing => quote! { ::embers_ricsl::__private::naga::BuiltIn::FrontFacing },
            BuiltinType::PrimitiveIndex => quote! { ::embers_ricsl::__private::naga::BuiltIn::PrimitiveIndex },
            BuiltinType::SampleIndex => quote! { ::embers_ricsl::__private::naga::BuiltIn::SampleIndex },
            BuiltinType::SampleMask => quote! { ::embers_ricsl::__private::naga::BuiltIn::SampleMask },
            BuiltinType::GlobalInvocationId => quote! { ::embers_ricsl::__private::naga::BuiltIn::GlobalInvocationId },
            BuiltinType::LocalInvocationId => quote! { ::embers_ricsl::__private::naga::BuiltIn::LocalInvocationId },
            BuiltinType::LocalInvocationIndex => quote! { ::embers_ricsl::__private::naga::BuiltIn::LocalInvocationIndex },
            BuiltinType::WorkGroupId => quote! { ::embers_ricsl::__private::naga::BuiltIn::WorkGroupId },
            BuiltinType::WorkGroupSize => quote! { ::embers_ricsl::__private::naga::BuiltIn::WorkGroupSize },
            BuiltinType::NumWorkGroups => quote! { ::embers_ricsl::__private::naga::BuiltIn::NumWorkGroups },
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
                        let ident = path.get_ident().ok_or_else(|| darling::Error::custom("unexpected path"))?.to_string();
                        Ok(BuiltinType::from_str(&ident).map_err(|e| darling::Error::custom(e))?)
                    },
                    Meta::List(list) => {
                        todo!()
                    },
                    Meta::NameValue(_) => todo!(),
                }
            },
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


pub fn process_entrypoint(input: &ItemFn) -> Result<TokenStream, Error> {
    let vis = &input.vis;

    let mut output = TokenBuffer::default();
    let mut name_gen = NameGen::default();

    let ident = &input.sig.ident;
    let ident_literal = ident_to_literal(&ident);

    output.push(quote! {
        let mut module_builder = ::embers_ricsl::__private::ModuleBuilder::default();
        let mut function_builder = module_builder.add_function(#ident_literal);
    });

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
                        output.push(quote! { let _binding = Some(::embers_ricsl::__private::naga::Binding::BuiltIn(#b)); });
                    },
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

                        let var = name_gen.var(pat_ident.ident.to_string(), pat_ident.span());
                        output.push(quote! {
                            let #var = function_builder.add_input_named::<#ty>(#name, #is_mut, _binding);
                        });
                    }
                    syn::Pat::Wild(_) => {
                        output.push(quote! {
                            function_builder.add_input_wild::<#ty>();
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

    output.push(quote! { function_builder.build(); });
    output.push(quote! { module_builder.build() });

    let generated = quote! {
        #vis fn #ident() -> ::embers_ricsl::__private::Module {
            #output
        }
    };

    Ok(generated)
}




fn process_block_inner(
    input: &Block,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<(), Error> {
    for stmt in &input.stmts {
        process_stmt(stmt, output, name_gen)?
    }
    Ok(())
}

fn process_stmt(
    input: &Stmt,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<(), Error> {
    match input {
        Stmt::Local(local) => {
            match &local.pat {
                Pat::Ident(lhs) => {
                    assert!(lhs.by_ref.is_none());
                    assert!(lhs.subpat.is_none());
                    let is_mut = lhs.mutability.is_some();

                    if let Some(rhs) = &local.init {
                        assert!(rhs.diverge.is_none());
                        process_expr(&rhs.expr, output, name_gen)?;
                    }
                    else {
                    }

                    todo!();
                }
                _ => panic!("unsupported"),
            }
        }
        Stmt::Item(_) => todo!(),
        Stmt::Expr(expr, _) => {
            let _ = process_expr(expr, output, name_gen)?;
        }
        Stmt::Macro(_) => todo!(),
    }

    Ok(())
}

#[must_use]
enum ExprOut {
    Ident(Ident),
    Path(Path),
}

impl From<Ident> for ExprOut {
    fn from(ident: Ident) -> Self {
        Self::Ident(ident)
    }
}

impl ToTokens for ExprOut {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ExprOut::Ident(ident) => tokens.extend(quote! { #ident }),
            ExprOut::Path(path) => tokens.extend(quote! { #path }),
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
                let #var = function_builder.add_expression::<$ty>(
                    ::embers_ricsl::__private::naga::Expression::Literal(
                        ::embers_ricsl::__private::naga::Literal::$variant(#x)
                    )
                );
            });
            ExprOut::from(var)
        }};
    }

    macro_rules! emit_func_call {
        ($func:expr, $args:expr) => {{
            let out = name_gen.tmp_var("call");
            let args: Vec<ExprOut> = $args.into();

            output.push(quote!{
                let _gen = $func(#(#args),*);
                let #out = function_builder.add_call(_gen, vec![#(#args),*]);
            });

            ExprOut::from(out)
        }};
    }

    let expr_out = match input {
        Expr::Assign(assign) => todo!(),
        Expr::Binary(binary) => todo!(),
        Expr::Block(block) => {
            assert!(block.attrs.is_empty());
            assert!(block.label.is_none());
            let mut block_output = TokenBuffer::default();
            let var = name_gen.tmp_var("block");
            process_block_inner(&block.block, &mut block_output, name_gen)?;
            output.push(quote! {
                let #var = {
                    #block_output
                };
            });
            var.into()
        }
        Expr::Break(brk) => todo!(),
        Expr::Call(call) => {
            let func = &call.func;
            let mut args = vec![];
            for arg in &call.args {
                let arg = process_expr(arg, output, name_gen)?;
                args.push(arg);
            }
            emit_func_call!(func, args)
        }
        Expr::Cast(cast) => todo!(),
        Expr::Closure(closure) => todo!(),
        Expr::Continue(cont) => todo!(),
        Expr::Field(field) => todo!(),
        Expr::ForLoop(for_) => todo!(),
        Expr::If(if_) => todo!(),
        Expr::Index(index) => todo!(),
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
        Expr::Loop(loop_) => todo!(),
        Expr::MethodCall(call) => todo!(),
        Expr::Paren(paren) => {
            assert!(paren.attrs.is_empty());
            process_expr(&paren.expr, output, name_gen)?
        }
        Expr::Path(path) => ExprOut::Path(path.path.to_owned()),
        Expr::Repeat(repeat) => todo!(),
        Expr::Return(ret) => todo!(),
        Expr::Struct(strct) => todo!(),
        Expr::Try(try_) => todo!(),
        Expr::Tuple(tuple) => todo!(),
        Expr::Unary(unary) => {
            let arg = process_expr(&unary.expr, output, name_gen)?;
            let out = match &unary.op {
                UnOp::Deref(_) => {
                    emit_func_call!(::embers_ricsl::__private::ops::Deref::deref, [arg])
                }
                UnOp::Not(_) => {
                    emit_func_call!(::embers_ricsl::__private::ops::Not::not, [arg])
                }
                UnOp::Neg(_) => {
                    emit_func_call!(::embers_ricsl::__private::ops::Neg::neg, [arg])
                }
                _ => todo!(),
            };
            out.into()
        }
        Expr::While(while_) => todo!(),
        _ => panic!("unsupported: {input:?}"),
    };

    Ok(expr_out)
}

fn ident_to_literal(ident: &Ident) -> Literal {
    Literal::string(&ident.to_string())
}
