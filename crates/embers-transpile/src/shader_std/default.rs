use embers_transpile_macros::transpile;


#[transpile]
pub trait Default {
    fn default() -> Self;
}


macro_rules! impl_default_with_zero_value {
    ($ty:ty) => {
        #[transpile]
        impl crate::shader_std::default::Default for $ty {
            fn default() -> $ty {
                ::embers_transpile::__private::intrinsic! {
                    let ty_handle = function_builder.module_builder.get_type_by_id_or_add_it::<Self>()?.try_get_data()?;
                    function_builder.add_expression::<Self>(crate::__private::naga::Expression::ZeroValue(ty_handle))?
                }
            }
        }
    };
}

pub(crate) use impl_default_with_zero_value;
