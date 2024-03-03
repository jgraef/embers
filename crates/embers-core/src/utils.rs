pub enum Assert<const CHECK: bool> {}
pub trait IsTrue {}
impl IsTrue for Assert<true> {}
pub trait IsFalse {}
impl IsFalse for Assert<false> {}

pub const fn max_rank(r1: usize, r2: usize) -> usize {
    if r1 > r2 {
        r1
    }
    else {
        r2
    }
}
