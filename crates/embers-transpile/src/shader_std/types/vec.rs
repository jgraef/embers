use std::marker::PhantomData;

use crate::{
    builder::{
        error::BuilderError,
        module::ModuleBuilder,
        r#type::{
            scalar_to_naga,
            ScalarKind,
            TypeHandle,
            Width,
        },
    },
    ShaderType,
};

#[allow(non_camel_case_types)]
pub struct vec<T, const N: usize> {
    _ty: PhantomData<T>,
}

macro_rules! impl_vec_n {
    ($n:expr, $shorthand:ident, $size:ident) => {
        impl<T: ScalarKind + Width + 'static> ShaderType for vec<T, $n> {
            fn add_to_module(
                module_builder: &mut ModuleBuilder,
            ) -> Result<TypeHandle, BuilderError> {
                Ok(module_builder.add_naga_type::<Self>(
                    Some(stringify!($shorthand).to_owned()),
                    naga::TypeInner::Vector {
                        size: naga::VectorSize::$size,
                        scalar: scalar_to_naga::<T>(),
                    },
                ))
            }
        }

        #[allow(non_camel_case_types)]
        pub type $shorthand<T> = vec<T, $n>;
    };
}

// todo: we could implement vectors (with arbitrary N) for empty types, but then
// we need a trait to mark empty types.

impl_vec_n!(2, vec2, Bi);
impl_vec_n!(3, vec3, Tri);
impl_vec_n!(4, vec4, Quad);

impl<T: Width, const N: usize> Width for vec<T, N> {
    const WIDTH: usize = <T as Width>::WIDTH * N;
}

#[allow(non_camel_case_types)]
pub struct mat<T, const N: usize, const M: usize> {
    _ty: PhantomData<T>,
}

macro_rules! impl_mat_n_m {
    ($n:expr, $m:expr, $shorthand:ident, $size_n:ident, $size_m:ident) => {
        impl<T: ScalarKind + Width + 'static> ShaderType for mat<T, $n, $m> {
            fn add_to_module(
                module_builder: &mut ModuleBuilder,
            ) -> Result<TypeHandle, BuilderError> {
                Ok(module_builder.add_naga_type::<Self>(
                    Some(stringify!($shorthand).to_owned()),
                    naga::TypeInner::Matrix {
                        columns: naga::VectorSize::$size_n,
                        rows: naga::VectorSize::$size_m,
                        scalar: scalar_to_naga::<T>(),
                    },
                ))
            }
        }

        #[allow(non_camel_case_types)]
        pub type $shorthand<T> = vec<T, $n>;
    };
}

impl_mat_n_m!(2, 2, mat2x2, Bi, Bi);
impl_mat_n_m!(2, 3, mat2x3, Bi, Tri);
impl_mat_n_m!(2, 4, mat2x4, Bi, Quad);
impl_mat_n_m!(3, 2, mat3x2, Tri, Bi);
impl_mat_n_m!(3, 3, mat3x3, Tri, Tri);
impl_mat_n_m!(3, 4, mat3x4, Tri, Quad);
impl_mat_n_m!(4, 2, mat4x2, Quad, Bi);
impl_mat_n_m!(4, 3, mat4x3, Quad, Tri);
impl_mat_n_m!(4, 4, mat4x4, Quad, Quad);

impl<T: Width, const N: usize, const M: usize> Width for mat<T, N, M> {
    const WIDTH: usize = <T as Width>::WIDTH * N;
}
