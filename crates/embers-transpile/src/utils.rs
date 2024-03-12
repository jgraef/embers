pub enum Assert<const CHECK: bool> {}
pub trait IsTrue {}
impl IsTrue for Assert<true> {}
pub trait IsFalse {}
impl IsFalse for Assert<false> {}

pub fn try_all<I: IntoIterator<Item = Result<bool, E>>, E>(iter: I) -> Result<bool, E> {
    for result in iter {
        if !result? {
            return Ok(false);
        }
    }
    Ok(true)
}

macro_rules! sealed_trait {
    ($mod_name:ident :: $trait_name:ident) => {
        mod $mod_name {
            pub trait $trait_name {}
        }
    };
    () => {
        crate::utils::sealed_trait!(sealed::Sealed);
        use sealed::Sealed;
    };
}

pub(crate) use sealed_trait;
