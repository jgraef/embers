// map.wgsl: {{ info.label }}

{% include "include/common.wgsl" %}

@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let index_range = index_range(global_id, num_workgroups);

    for (var index = index_range.x; index < index_range.y; index += {{ index_step }}) {

        // read inputs

        {% for input in inputs %}
            let index_{{ input }} = p_{{ input }}_offset()
            + project(
                index,
                P_OP_STRIDES,
                P_OP_SHAPE,
                P_{{ input|upper }}_STRIDES
            );

            {% if map_encoded %}
                let value_{{ input }} = b_{{ input }}_read_encoded(index_{{ input }});
            {% else %}
                let value_{{ input }} = b_{{ input }}_decode(index_{{ input }});
            {% endif %}
        {% endfor %}

        // perform the actual operation

        {{ body }}

        // write outputs

        {% for output in outputs %}
            let index_{{ output }} = p_{{ output }}_offset()
            + project(
                index,
                P_OP_STRIDES,
                P_OP_SHAPE,
                P_{{ output|upper }}_STRIDES
            );

            {% if map_encoded %}
                b_{{ output }}_write_encoded(index_{{ output }}, value_{{ output }});
            {% else %}
                b_{{ output }}_encode(index_{{ output }}, value_{{ output }});
            {% endif %}
        {% endfor %}
    }
}
