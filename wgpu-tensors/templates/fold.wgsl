// reduce.wgsl: {{ info.label }}

{% include "common.wgsl" %}


@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let index_range = index_range(global_id, num_workgroups);

    let reducer_size = shape_size(P_REDUCER_SHAPE);

    for (var index = index_range.x; index < index_range.y; index++) {

        // input projection

        let input_index = p_input_offset()
            + project(
                index,
                P_REDUCED_STRIDES,
                P_REDUCED_SHAPE,
                P_INPUT_STRIDES
            );


        // reduction

        {% for var in state %}
            var {{ var.name }}: {{ var.ty }} = {{ var.init }};
        {% endfor %}

        for (var reducer_i = 0; reducer_i < reducer_size; reducer_i++) {
            // project along reduction axis
            let reducer_index = input_index
                + project(
                    reducer_i,
                    P_REDUCER_STRIDES,
                    P_REDUCER_SHAPE,
                    P_INPUT_STRIDES,
                );

            // get input
            let value_input = b_input_decode(reducer_index);

            // apply folding function
            {{ apply }}
        }


        // write output

        {{ epilog }}

        let result_index = p_result_offset()
            + project(
                index,
                P_REDUCED_STRIDES,
                P_REDUCED_SHAPE,
                P_RESULT_STRIDES,
            );

        b_result_encode(result_index, value_result);
    }
}