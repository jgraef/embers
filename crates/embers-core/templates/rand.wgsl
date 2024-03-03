// prng.wgsl: {{ info.label }}
//
// PRNG inspired from [1]. This is a combination of Tausworthe and a LCG.
//
// [1]: https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-37-efficient-random-number-generation-and-application

{% include "include/common.wgsl" %}
{% include "include/prng.wgsl" %}

@compute
@workgroup_size({{ info.work_group_size.x }}, {{ info.work_group_size.y }}, {{ info.work_group_size.z }})
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let index_range = index_range(global_id, num_workgroups);

    // initialize prng from seed parameter and global id.
    var prng = prng_from_param_and_global_id(P_SEED, global_id);

    // prepare
    {{ prepare }}

    for (var index = index_range.x; index < index_range.y; index += {{ index_step }}) {

        // sample operation, specified in kernel
        {{ sample }}

        // write outputs

        let index_output = p_output_offset()
        + project(
            index,
            P_OP_STRIDES,
            P_OP_SHAPE,
            P_OUTPUT_STRIDES
        );

        {% if sample_encoded %}
            b_output_write_encoded(index_output, sampled);
        {% else %}
            b_output_encode(index_output, sampled);
        {% endif %}

    }
}
