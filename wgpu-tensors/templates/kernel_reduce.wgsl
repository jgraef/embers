// Kernel: {{ label }}

{% for binding in bindings %}
@group(0)
@binding({{ binding.binding_id }})
var<storage, {{ binding.rw }}> {{ binding.name }}: array<{{ binding.data_type }}>;
{% endfor %}

@group(0)
@binding({{ info_binding_id }})
var<storage, read> info_data: array<i32>;

const INTERMEDIATE_SIZE: u32 = {{ intermediate.size }}u;
var<workgroup> intermediate: array<{{ intermediate.data_type }}, INTERMEDIATE_SIZE>;


const WORKGROUP_SIZE_X = {{ work_group_size.x }}u;
const REDUCE_THREADS = {{ reduce_threads }}u;

fn dim() -> u32 {
    return u32(info_data[0u]);
}

fn chunk_size() -> u32 {
    return u32(info_data[1u]);
}

fn offset(binding: u32) -> i32 {
    return info_data[2u * binding * dim() + binding + 2u];
}

fn shape(binding: u32, i: u32) -> i32 {
    return info_data[2u * binding * dim() + binding + i + 3u];
}

fn stride(binding: u32, i: u32) -> i32 {
    return info_data[2u * binding * dim() + binding + dim() + i + 3u];
}

@compute
@workgroup_size({{ work_group_size.x }}, {{ work_group_size.y }}, 1)
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(local_invocation_id) local_id: vec3<u32>,
    @builtin(num_workgroups) num_workgroups: vec3<u32>,
) {
    let id = global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x;

    {% for binding in bindings %}
        var index_{{ binding.name }}: i32 = offset({{ loop.index }}u);
    {% endfor %}

    for (var i: u32 = 0u; i < dim(); i++) {
        let op_stride = stride(0u, i);
        let x = i32(id) / op_stride;

        {% for binding in bindings %}
            let stride_{{ binding.name }} = stride({{ loop.index }}u, i);
            let shape_{{ binding.name }} = shape({{ loop.index }}u, i);
            index_{{ binding.name }} += x % shape_{{ binding.name }} * stride_{{ binding.name }};
        {% endfor %}
    }

    {{ body }}
}
