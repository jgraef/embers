// reduce.wgsl: {{ info.label }}

{% include "common.wgsl" %}


@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let id = i32(global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x);

    let reducer_size = shape_size(P_REDUCER_SHAPE);


    // input projection

    let input_offset = p_input_offset()
        + project(
            id,
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
        let reducer_offset = input_offset
            + project(
                reducer_i,
                P_REDUCER_STRIDES,
                P_REDUCER_SHAPE,
                P_INPUT_STRIDES,
            );

        // get input
        let x = input[reducer_offset];

        // apply folding function
        {{ apply }}
    }


    // write output

    let result_offset = p_result_offset()
        + project(
            id,
            P_REDUCED_STRIDES,
            P_REDUCED_SHAPE,
            P_RESULT_STRIDES,
        );

    {{ epilog }}
}