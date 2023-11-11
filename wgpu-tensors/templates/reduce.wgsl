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
    @builtin(local_invocation_id) local_id: vec3<u32>,
    @builtin(workgroup_id) workgroup_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let global_id = global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x;
    let local_id = local_id.x * NUM_THREADS + local_id.y;

    // calculate the buffer indices for the tensor for the given global id

    {% for binding in info.bindings %}
        var index_{{ binding.name }}: i32 = binding_offset({{ loop.index - 1 }}u);
    {% endfor %}

    for (var axis: u32 = 0u; axis < dim(); axis++) {
        let x = i32(global_id) / op_stride(axis);

        {% for binding in info.bindings %}
            index_{{ binding.name }} += x % op_shape(axis) * binding_stride({{ loop.index - 1 }}u, axis);
        {% endfor %}
    }

    // initialize reducer state
    var reduce_state: {{ state_type }} = {{ state_init }};
    let reduce_n = reducer_size();

    for (var reduce_i: i32 = 0; reduce_i < reduce_n; reduce_i++) {
        var reducer_offset: i32 = index_{{ input_name }};

        for (var axis: u32 = 0u; axis < dim(); axis++) {
            reducer_offset += i32(reduce_i) / reducer_stride(0, axis) * binding_stride({{ input_index }}u);
        }

        {{ func }}
    }

    // todo
    result[index_result] = reduce_state;
}