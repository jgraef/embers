use std::fmt::Display;

use proc_macro2::{
    Ident,
    Literal,
    Span,
    TokenStream,
};
use quote::{
    quote,
    ToTokens,
};
use syn::{
    parse::Parse,
    Lifetime,
    Type,
    TypePath,
};

pub fn ident_to_literal(ident: &Ident) -> Literal {
    Literal::string(&ident.to_string())
}

#[derive(Default)]
pub struct TokenBuffer {
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

pub fn map_types(ty: &mut Type) {
    match ty {
        Type::Array(type_array) => map_types(&mut type_array.elem),
        Type::BareFn(bare_fn) => todo!(),
        Type::Group(group) => map_types(&mut group.elem),
        Type::ImplTrait(_) => {}
        Type::Infer(_) => {}
        Type::Macro(_) => {}
        Type::Never(_) => {}
        Type::Paren(paren) => map_types(&mut paren.elem),
        Type::Path(path) => {
            if let Some(qself) = &mut path.qself {
                map_types(&mut qself.ty);
            }
        }
        Type::Ptr(_) => panic!("pointers are not supported"),
        Type::Reference(reference) => {
            let ty = &reference.elem;
            let space = map_lifetime_to_address_space(reference.lifetime.as_ref());
            let ptr_type = syn::parse2::<Type>(quote! {
                ::embers_transpile::__private::Pointer<#ty, #space>
            })
            .unwrap();
        }
        Type::Slice(slice) => map_types(&mut slice.elem),
        Type::TraitObject(_) => panic!("trait objects are not supported"),
        Type::Tuple(tuple) => {
            for elem in tuple.elems.iter_mut() {
                map_types(elem);
            }
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
