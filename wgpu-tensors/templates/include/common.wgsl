{% include "include/bindings.wgsl" %}
{% include "include/parameters.wgsl" %}

// constants

const WORKGROUP_SIZE_X = {{ info.work_group_size.x }}u;
const WORKGROUP_SIZE_Y = {{ info.work_group_size.y }}u;
const WORKGROUP_SIZE_Z = {{ info.work_group_size.z }}u;
const WORKGROUP_SIZE = {{ info.work_group_size.product() }}u;

const PI = 3.14159265358979323846264338327950288;



// helper functions

fn project(input: i32, p_stride_in: u32, p_shape: u32, p_stride_out: u32) -> i32 {
    var output = 0;
    for (var axis = 0u; axis < dim(); axis++) {
        let stride_in = p_array(p_stride_in, axis);
        if stride_in != 0 {
            output += (input / stride_in) % p_array(p_shape, axis) * p_array(p_stride_out, axis);
        }
    }
    return output;
}

fn shape_size(p_shape: u32) -> i32 {
    var size = 1;
    for (var axis = 0u; axis < dim(); axis++) {
        size *= p_array(p_shape, axis);
    }
    return size;
}

fn index_range(global_id: vec3<u32>, num_workgroups: vec3<u32>) -> vec2<i32> {
    // todo: use all 3 workgroup dimensions.
    let id = global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x;
    let start_index = i32(id * p_chunk_size());
    let end_index = start_index + i32(p_chunk_size());
    return vec2<i32>(start_index, end_index);
}


// block types

struct packed_bool_t { b: u32 }
struct packed_i8_t { i: u32 }
struct packed_u8_t { i: u32 }
struct packed_i16_t { i: u32 }
struct packed_u16_t { i: u32 }
