// reduce.wgsl: {{ info.label }}

{% include "common.wgsl" %}

/*{#
const NUM_THREADS = {{ num_threads }}u;
var<workgroup> reducer_state: array<{{ state.data_type }}, NUM_THREADS>;
#}*/

@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    //@builtin(local_invocation_id) local_id: vec3<u32>,
    //@builtin(workgroup_id) workgroup_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let id = i32(global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x);
    //let local_id = local_id.x * NUM_THREADS + local_id.y;

    let reduced_size = shape_size(P_REDUCED_SHAPE);
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

    var reducer_state: {{ state.ty }} = {{ state.init }};

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
        //reducer_state = f(reducer_state, x);
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

    result[result_offset] = reducer_state;
}