use darling::{
    FromAttributes,
    FromMeta,
};
use syn::{
    Attribute,
    ExprClosure,
};

use crate::{
    error::Error,
    expression::ExprOut,
    utils::{
        NameGen,
        TokenBuffer,
    },
};

#[derive(Debug, FromAttributes)]
#[darling(attributes(transpile))]
pub struct ClosureAttributes {
    #[darling(flatten)]
    pub meta: ClosureMeta,
}

#[derive(Debug, FromMeta)]
pub struct ClosureMeta {
    #[darling(default)]
    pub inline: bool,
}

impl ClosureMeta {
    pub fn parse(item_attributes: &[Attribute]) -> Result<Self, Error> {
        let meta = ClosureAttributes::from_attributes(item_attributes)?.meta;
        Ok(meta)
    }
}

pub fn process_closure(
    closure: &ExprClosure,
    output: &mut TokenBuffer,
    name_gen: &mut NameGen,
) -> Result<ExprOut, Error> {
    let attributes = ClosureMeta::parse(&closure.attrs)?;

    let type_ident = name_gen.tmp_var("closure");

    /*output.push(quote!{
        {
            struct #ident;

            pub trait Fn<Args>: FnMut<Args> {
                extern "rust-call" fn call(&self, args: Args) -> Self::Output;
            }
        }
    });*/

    todo!();
}
