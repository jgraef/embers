mod utils;
use embers_transpile::shader_std::prelude::*;
use utils::*;

#[test]
fn vec_alignment() {
    // https://sotrh.github.io/learn-wgpu/showcase/alignment/#alignment-of-vertex-and-index-buffers
    assert_type_alignment::<vec2<u32>>(8);
    assert_type_alignment::<vec3<u32>>(16);
    assert_type_alignment::<vec4<u32>>(16);
}

#[test]
fn vec_size() {
    // https://sotrh.github.io/learn-wgpu/showcase/alignment/#alignment-of-vertex-and-index-buffers
    assert_type_width::<vec2<u32>>(8);
    assert_type_width::<vec3<u32>>(12);
    assert_type_width::<vec4<u32>>(16);
}
