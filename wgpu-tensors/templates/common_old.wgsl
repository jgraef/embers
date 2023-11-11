{% for binding in info.bindings %}
@group(0)
@binding({{ binding.binding_id }})
var<storage, {{ binding.rw }}> {{ binding.name }}: array<{{ binding.data_type }}>;
{% endfor %}

@group(0)
@binding({{ info.info_binding_id }})
var<storage, read> info_data: array<i32>;

const WORKGROUP_SIZE_X = {{ info.work_group_size.x }}u;
const WORKGROUP_SIZE_Y = {{ info.work_group_size.y }}u;
const WORKGROUP_SIZE_Z = {{ info.work_group_size.z }}u;
const WORKGROUP_SIZE = {{ info.work_group_size.product() }}u;

// num bindings excluding the info_data binding
const NUM_BINDINGS = {{ info.bindings.len() }}u;
const NUM_INFO_CONSTANTS = 2u;

fn dim() -> u32 {
    return u32(info_data[0u]);
}

fn chunk_size() -> u32 {
    return u32(info_data[1u]);
}

fn info_offset(index: u32) -> i32 {
    return info_data[2u * index * dim() + index + NUM_INFO_CONSTANTS];
}

fn info_shape(index: u32, axis: u32) -> i32 {
    return info_data[2u * index * dim() + index + axis + NUM_INFO_CONSTANTS + 1u];
}

fn info_stride(index: u32, axis: u32) -> i32 {
    return info_data[2u * index * dim() + index + dim() + axis + NUM_INFO_CONSTANTS + 1u];
}

fn op_size() -> i32 {
    return info_offset(0u);
}

fn op_shape(axis: u32) -> i32 {
    return info_shape(0u, axis);
}

fn op_stride(axis: u32) -> i32 {
    return info_stride(0u, axis);
}

fn binding_offset(index: u32) -> i32 {
    return info_offset(index + 1u);
}

fn binding_shape(index: u32, axis: u32) -> i32 {
    return info_shape(index + 1u, axis);
}

fn binding_stride(index: u32, axis: u32) -> i32 {
    return info_stride(index + 1u, axis);
}

fn reducer_size(index: u32) -> i32 {
    return info_offset(NUM_BINDINGS + index)
}

fn reducer_shape(index: u32, axis: u32) -> i32 {
    return info_shape(NUM_BINDINGS + index, axis);
}

fn reducer_stride(index: u32, axis: u32) -> i32 {
    return info_stride(NUM_BINDINGS + index, axis);
}
