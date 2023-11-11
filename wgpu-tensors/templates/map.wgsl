// map.wgsl: {{ info.label }}

{% include "common.wgsl" %}

@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let id = i32(global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x);

    // calculate the buffer indices for the tensor for the given global id

    {% for binding in info.declaration.bindings %}
        let index_{{ binding.name }} = p_{{ binding.name }}_offset()
        + project(
            id,
            P_OP_STRIDES,
            P_OP_SHAPE,
            P_{{ binding.name|upper }}_STRIDES
        );
    {% endfor %}

    // perform the actual operation

    {{ body }}
}
