use std::fmt::Debug;

use bytemuck::Pod;

pub trait Element: Copy + Debug + Pod {
    const WGSL_TYPE: &'static str;
    const ZERO: Self;
    const ONE: Self;
}

pub trait Number {}

impl Element for f32 {
    const WGSL_TYPE: &'static str = "f32";
    const ZERO: Self = 0.;
    const ONE: Self = 1.;
}

impl Number for f32 {}

impl Element for i32 {
    const WGSL_TYPE: &'static str = "i32";
    const ZERO: Self = 0;
    const ONE: Self = 1;
}

impl Number for i32 {}
