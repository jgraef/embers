use std::fmt::Display;

use proc_macro2::{
    Ident,
    Literal,
    Span,
    TokenStream,
};
use quote::{
    quote,
    quote_spanned,
    ToTokens,
};
use syn::{
    spanned::Spanned,
    Lifetime,
    Type,
};

pub fn ident_to_literal(ident: &Ident) -> Literal {
    let mut literal = Literal::string(&ident.to_string());
    literal.set_span(ident.span());
    literal
}

#[derive(Default)]
pub struct TokenBuffer {
    buf: TokenStream,
}

impl TokenBuffer {
    pub fn finish(self) -> TokenStream {
        self.buf
    }
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
pub struct NameGen {
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
}

#[derive(Clone, Copy, Debug)]
pub enum TypePosition {
    Argument,
    ReturnType,
    Path,
    Let,
}

pub fn map_types(ty: &mut Type, position: TypePosition) {
    match ty {
        Type::Array(type_array) => {
            let mut elem = type_array.elem.clone();
            map_types(&mut elem, position);
            let len = type_array.len.clone();
            let span = ty.span();
            *ty = syn::parse2::<Type>(quote_spanned! {span=>
                ::embers_transpile::__private::Array<#elem, { #len }>
            })
            .unwrap();
        }
        Type::BareFn(_bare_fn) => todo!(),
        Type::Group(group) => map_types(&mut group.elem, position),
        Type::ImplTrait(_) => {}
        Type::Infer(_) => {}
        Type::Macro(_) => {}
        Type::Never(_) => {}
        Type::Paren(paren) => map_types(&mut paren.elem, position),
        Type::Path(path) => {
            if let Some(qself) = &mut path.qself {
                map_types(&mut qself.ty, TypePosition::Path);
            }
        }
        Type::Ptr(_) => panic!("pointers are not supported"),
        Type::Reference(reference) => {
            let mut elem = reference.elem.clone();
            map_types(&mut elem, position);
            let space = match position {
                TypePosition::Argument => {
                    quote! {
                        impl ::embers_transpile::__private::AddressSpace
                    }
                }
                TypePosition::Path => quote! { _ },
                _ => map_lifetime_to_address_space(reference.lifetime.as_ref()),
            };
            let span = ty.span();
            *ty = syn::parse2::<Type>(quote_spanned! {span=>
                ::embers_transpile::__private::Pointer<#elem, #space>
            })
            .unwrap();
        }
        Type::Slice(slice) => {
            let mut elem = slice.elem.clone();
            map_types(&mut elem, position);
            let span = ty.span();
            *ty = syn::parse2::<Type>(quote_spanned! {span=>
                ::embers_transpile::__private::DynamicArray<#elem>
            })
            .unwrap();
        }
        Type::TraitObject(_) => panic!("trait objects are not supported"),
        Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                let span = ty.span();
                *ty = syn::parse2::<Type>(quote_spanned! {span=>
                    ::embers_transpile::__private::Unit
                })
                .unwrap();
            }
            //for elem in tuple.elems.iter_mut() {
            //    map_types(elem, position);
            //}
            todo!();
        }
        Type::Verbatim(_) => {}
        _ => todo!(),
    }
}

pub fn map_lifetime_to_address_space(lt_opt: Option<&Lifetime>) -> TokenStream {
    if let Some(lt) = lt_opt {
        match lt.ident.to_string().as_str() {
            "function" => {
                quote! {
                    ::embers_transpile::__private::address_space::Function
                }
            }
            "private" => {
                quote! {
                    ::embers_transpile::__private::address_space::Private
                }
            }
            "workgroup" | "work_group" => {
                quote! {
                    ::embers_transpile::__private::address_space::WorkGroup
                }
            }
            "uniform" => {
                quote! {
                    ::embers_transpile::__private::address_space::Uniform
                }
            }
            "handle" => {
                quote! {
                    ::embers_transpile::__private::address_space::Handle
                }
            }
            "pushconstant" | "push_constant" => {
                quote! {
                    ::embers_transpile::__private::address_space::PushConstant
                }
            }
            _ => todo!(),
        }
    }
    else {
        quote! {
            ::embers_transpile::__private::address_space::Private
        }
    }
}
