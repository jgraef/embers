// map.wgsl: {{ info.label }}

{% include "common.wgsl" %}

@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let index_range = index_range(global_id, num_workgroups);

    for (var index = index_range.x; index < index_range.y; index++) {

        // calculate the buffer indices for the tensors for the given global id

        {% for input in inputs %}
            let index_{{ input }} = p_{{ input }}_offset()
            + project(
                index,
                P_OP_STRIDES,
                P_OP_SHAPE,
                P_{{ input|upper }}_STRIDES
            );
            let value_{{ input }} = b_{{ input }}_decode(index_{{ input }});
        {% endfor %}

        // perform the actual operation

        {{ body }}

        // write output

        {% for output in outputs %}
            let index_{{ output }} = p_{{ output }}_offset()
            + project(
                index,
                P_OP_STRIDES,
                P_OP_SHAPE,
                P_{{ output|upper }}_STRIDES
            );
            b_{{ output }}_encode(index_{{ output }}, value_{{ output }});
        {% endfor %}
    }
}
