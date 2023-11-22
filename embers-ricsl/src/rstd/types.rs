use crate::{
    arena::Handle,
    builder::{
        ModuleBuilder,
        Type,
    },
    RicslType,
};

macro_rules! impl_scalar {
    ($ty:ident, $name:expr, $kind:ident, $width:expr) => {
        impl RicslType for $ty {
            fn add_to_module(module_builder: &mut ModuleBuilder) -> Handle<Type> {
                module_builder.add_intrinsic_type::<Self>(
                    $name,
                    naga::TypeInner::Scalar {
                        kind: naga::ScalarKind::$kind,
                        width: $width,
                    },
                )
            }
        }
    };
}

impl_scalar!(i32, "i32", Sint, 4);
impl_scalar!(u32, "u32", Uint, 4);
#[cfg(feature = "half")]
impl_scalar!(half::f16, "f16", Float, 2);
impl_scalar!(f32, "f32", Float, 4);
impl_scalar!(f64, "f64", Float, 8);
impl_scalar!(bool, "bol", Bool, 1);

impl RicslType for () {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> Handle<Type> {
        todo!();
    }
}
