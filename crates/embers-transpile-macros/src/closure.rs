use proc_macro2::TokenStream;
use syn::{parenthesized, parse::{Parse, ParseStream}, punctuated::Punctuated, token::{Move, Paren}, Ident, Pat, Token};

use crate::error::Error;


pub struct Capture {
    pub move_token: Token![move],
    pub paren_token: Paren,
    pub vars: Punctuated<Ident, Token![,]>,
}

impl Parse for Capture {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let content;
        Ok(Self {
            move_token: input.parse()?,
            paren_token: parenthesized!(content in input),
            vars: content.parse_terminated(Ident::parse, Token![,])?,
        })
    }
}



pub struct Closure {
    pub capture: Option<Capture>,
    pub or1_token: Token![|],
    pub args: Punctuated<Pat, Token![,]>,
    pub or2_token: Token![|],
}

impl Parse for Closure {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let lookahead = input.lookahead1();
        let capture = if lookahead.peek(Token![move]) {
            Some(Capture::parse(input)?)
        }
        else {
            None
        };
        let or1_token = input.parse()?;
        let args = input.parse_terminated(Pat::parse_single, Token![,])?;
        let or2_token = input.parse()?;

        Ok(Self {
            capture,
            or1_token,
            args,
            or2_token,
        })
    }
}

impl Closure {
    pub fn process(&self) -> Result<TokenStream, Error> {
        todo!();
    }
}