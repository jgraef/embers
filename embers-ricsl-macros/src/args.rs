use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::quote;



#[derive(Debug, FromMeta)]
pub struct FnArgs {
    #[darling(default)]
    pub entrypoint: bool,
    #[darling(default)]
    pub __std: bool,
}


#[derive(Debug, FromMeta)]
pub struct TraitArgs {
    #[darling(default)]
    pub __std: bool,
}


pub trait StdFlag {
    fn is_std(&self) -> bool;
    fn private(&self) -> TokenStream {
        if self.is_std() {
            quote!{ crate::__private }
        }
        else {
            quote!{ ::embers_ricsl::__private }
        }
    }
}

impl StdFlag for FnArgs {
    fn is_std(&self) -> bool {
        self.__std
    }
}

impl StdFlag for TraitArgs {
    fn is_std(&self) -> bool {
        self.__std
    }
}