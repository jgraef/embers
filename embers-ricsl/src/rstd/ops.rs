use embers_ricsl_macros::ricsl;

#[ricsl(__std)]
pub trait Neg: Sized {
    type Output;

    fn neg(self) -> Self::Output;
}
