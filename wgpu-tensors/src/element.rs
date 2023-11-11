use std::fmt::{
    Debug,
    Display,
};

use bytemuck::Pod;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WgslType {
    Bool,
    I32,
    U32,
    F32,
    F16,
}

impl Display for WgslType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            WgslType::Bool => "bool",
            WgslType::I32 => "i32",
            WgslType::U32 => "u32",
            WgslType::F32 => "f32",
            WgslType::F16 => "f16",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WgslValue {
    Bool(bool),
    I32(i32),
    U32(u32),
    F32(f32),
    F16(f32),
}

impl WgslValue {
    fn wgsl_type(&self) -> WgslType {
        match self {
            WgslValue::Bool(_) => WgslType::Bool,
            WgslValue::I32(_) => WgslType::I32,
            WgslValue::U32(_) => WgslType::U32,
            WgslValue::F32(_) => WgslType::F32,
            WgslValue::F16(_) => WgslType::F16,
        }
    }
}

impl From<bool> for WgslValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<i32> for WgslValue {
    fn from(value: i32) -> Self {
        Self::I32(value)
    }
}

impl From<u32> for WgslValue {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}

impl From<f32> for WgslValue {
    fn from(value: f32) -> Self {
        Self::F32(value)
    }
}

impl Display for WgslValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WgslValue::Bool(value) => write!(f, "{value}"),
            WgslValue::I32(value) => write!(f, "{value}i"),
            WgslValue::U32(value) => write!(f, "{value}u"),
            WgslValue::F32(value) => write!(f, "{value}f"),
            WgslValue::F16(value) => write!(f, "f16({value}f)"),
        }
    }
}

pub trait Element: Copy + Debug + Pod + Into<WgslValue> {
    const WGSL_TYPE: WgslType;
    const ZERO: Self;
    const ONE: Self;
}

pub trait Number {}

impl Element for f32 {
    const WGSL_TYPE: WgslType = WgslType::F32;
    const ZERO: Self = 0.;
    const ONE: Self = 1.;
}

impl Number for f32 {}

impl Element for i32 {
    const WGSL_TYPE: WgslType = WgslType::I32;
    const ZERO: Self = 0;
    const ONE: Self = 1;
}

impl Number for i32 {}
