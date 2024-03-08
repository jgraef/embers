use embers_transpile_macros::transpile;

use crate::shader_std::types::primitive::bool;

#[transpile]
struct Option<T> {
    discriminant: bool,
    value: T,
}

//#[transpile]
//impl<T: Default + ShaderType> Default for Option<T> {
//    fn default() -> Self {
//        //None
//    }
//}

//#[transpile]
//fn None<T: ShaderType>(value: T) -> Option<T> {
//    Option {
//        discriminant: false,
//        value: Default::default(),
//    }
//}
