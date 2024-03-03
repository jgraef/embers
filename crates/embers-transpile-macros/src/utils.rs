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
