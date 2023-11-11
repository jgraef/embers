// map.wgsl: {{ info.label }}

{% include "common.wgsl" %}

@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let global_id = global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x;
    if global_id >= op_size() {
        return;
    }

    // calculate the buffer indices for the tensor for the given global id

    {% for binding in info.bindings %}
        var index_{{ binding.name }}: i32 = binding_offset({{ loop.index - 1 }}u);
    {% endfor %}

    for (var axis: u32 = 0u; axis < dim(); axis++) {
        let x = i32(global_id) / op_stride(axis);

        {% for binding in info.bindings %}
            index_{{ binding.name }} += x % binding_shape({{ loop.index - 1 }}u, axis) * binding_stride({{ loop.index - 1 }}u, axis);
        {% endfor %}
    }

    // perform the actual operation

    {{ body }}
}
