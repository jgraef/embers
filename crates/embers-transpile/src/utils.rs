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
