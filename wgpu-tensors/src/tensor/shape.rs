pub trait Shape {
    fn size(&self) -> usize;
    fn is_empty(&self) -> bool;
}

impl<const D: usize> Shape for [usize; D] {
    fn size(&self) -> usize {
        if D == 0 {
            0
        }
        else {
            self.iter().product()
        }
    }

    fn is_empty(&self) -> bool {
        D == 0 || self.iter().any(|x| *x == 0)
    }
}

impl<const D: usize> Shape for &[usize; D] {
    fn size(&self) -> usize {
        if D == 0 {
            0
        }
        else {
            self.iter().product()
        }
    }

    fn is_empty(&self) -> bool {
        D == 0 || self.iter().any(|x| *x == 0)
    }
}
