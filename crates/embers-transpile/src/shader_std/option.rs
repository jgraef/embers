use embers_transpile_macros::transpile;

use crate::{
    builder::r#type::{
        AlignTo,
        ShaderType,
        Width,
    },
    shader_std::{
        default::Default,
        types::primitive::bool,
    },
};

#[transpile]
struct Option<T> {
    discriminant: bool,
    value: T,
}

//#[transpile]
//impl<T: Default + ShaderType + Width + AlignTo> Default for Option<T> {
//    fn default() -> Self {
//        Self {
//            discriminant: false,
//            value: Default::default(),
//        }
//    }
//}

//#[transpile]
//fn None<T: ShaderType>(value: T) -> Option<T> {
//    Option {
//        discriminant: false,
//        value: Default::default(),
//    }
//}
