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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Vec3 {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}

impl Vec3 {
    pub fn product(&self) -> u32 {
        self.x * self.y * self.z
    }
}
