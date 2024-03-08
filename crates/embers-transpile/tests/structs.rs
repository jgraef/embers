mod utils;
use embers_transpile::{
    shader_std::prelude::*,
    transpile,
};
use utils::*;

#[test]
fn it_doesnt_generate_a_type_for_empty_structs() {
    fn assert_no_types_generated<T: ShaderType>() {
        let module = module_for_type::<T>();
        assert!(module.types.is_empty());
    }

    #[transpile]
    struct A;
    assert_no_types_generated::<A>();

    #[transpile]
    struct B {}
    assert_no_types_generated::<B>();

    #[transpile]
    struct C();
    assert_no_types_generated::<C>();

    #[transpile]
    struct D {
        a: A,
        b: B,
        c: C,
    }
    assert_no_types_generated::<D>();
}

#[test]
fn it_aligns_fields_correctly() {
    #[transpile]
    struct A {
        a: u32,
        b: vec3<u32>,
        c: u32,
    }
    assert_struct_alignment::<A>(&[0, 16, 28]);
}

#[test]
fn it_sizes_structs_correctly() {
    #[transpile]
    struct A {
        a: u32,
        b: i32,
    }
    assert_struct_size::<A>(8);

    #[transpile]
    struct B {
        a: u32,
        b: f64,
    }
    assert_struct_size::<B>(16);
}
