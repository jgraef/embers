// parameter binding
@group(0)
@binding(0)
var<storage, read> parameters: array<i32>;

// tensor bindings
{% for binding in info.declaration.bindings %}
    @group(0)
    @binding({{ loop.index }})
    var<storage, {{ binding.read_write }}> _b_{{ binding.name }}_encoded: array<{{ binding.encoding.ty }}>;

    const B_{{ binding.name|upper }}_NUM_PACKED: i32 = {{ binding.encoding.num_packed }}i;

    fn b_{{ binding.name }}_read_encoded(index: i32) -> {{ binding.encoding.ty }} {
        return _b_{{ binding.name }}_encoded[index / B_{{ binding.name|upper }}_NUM_PACKED];
    }

    fn b_{{ binding.name }}_decode(index: i32) -> {{ binding.ty }} {
        let encoded = b_{{ binding.name }}_read_encoded(index);
        {{ binding.encoding.decode }}
        return value;
    }

    {% match binding.read_write %}
        {% when crate::kernel::binding::KernelBindingReadWrite::ReadWrite %}
            fn b_{{ binding.name }}_write_encoded(index: i32, value: {{ binding.encoding.ty }}) {
                _b_{{ binding.name }}_encoded[index / B_{{ binding.name|upper }}_NUM_PACKED] = value;
            }

            var<private> _b_{{ binding.name }}_encode_buffer: {{ binding.encoding.ty }};

            fn b_{{ binding.name }}_encode(index: i32, value: {{ binding.ty }}) {
                let i = index % B_{{ binding.name|upper }}_NUM_PACKED;
                let output = &_b_{{ binding.name }}_encoded[index / B_{{ binding.name|upper }}_NUM_PACKED];
                {{ binding.encoding.encode }}
            }
        {% else %}
    {% endmatch %}
{% endfor %}

// num bindings excluding the parameter binding
const B_COUNT = {{ info.declaration.bindings.len() }}u;

// constants

const WORKGROUP_SIZE_X = {{ info.work_group_size.x }}u;
const WORKGROUP_SIZE_Y = {{ info.work_group_size.y }}u;
const WORKGROUP_SIZE_Z = {{ info.work_group_size.z }}u;
const WORKGROUP_SIZE = {{ info.work_group_size.product() }}u;

// parameters

const P_COUNT = {{ info.declaration.parameters.len() }}u;

fn dim() -> u32 {
    return u32(parameters[0u]);
}

fn p_chunk_size() -> u32 {
    return u32(parameters[1u]);
}

fn p_int(i: u32) -> i32 {
    return parameters[i + 2u];
}

fn p_shaped(i: u32, axis: u32) -> i32 {
    let p = u32(p_int(i)) + P_COUNT + 2u;
    return parameters[u32(p) + axis];
}

{% for parameter in info.declaration.parameters %}
    const P_{{ parameter.name|upper }}: u32 = {{ loop.index - 1 }}u;

    {% match parameter.ty %}
        {% when crate::kernel::binding::KernelParameterType::Int %}
            fn p_{{ parameter.name }}() -> i32 {
                return p_int({{ loop.index - 1 }}u);
            }
        {% when crate::kernel::binding::KernelParameterType::Shaped %}
            fn p_{{ parameter.name }}(axis: u32) -> i32 {
                return p_shaped({{ loop.index - 1 }}u, axis);
            }
        {% when crate::kernel::binding::KernelParameterType::Strider %}
    {% endmatch %}
{% endfor %}


// helper functions

fn project(input: i32, p_stride_in: u32, p_shape: u32, p_stride_out: u32) -> i32 {
    var output = 0;
    for (var axis = 0u; axis < dim(); axis++) {
        let stride_in = p_shaped(p_stride_in, axis);
        if stride_in != 0 {
            output += (input / stride_in) % p_shaped(p_shape, axis) * p_shaped(p_stride_out, axis);
        }
    }
    return output;
}

fn shape_size(p_shape: u32) -> i32 {
    var size = 1;
    for (var axis = 0u; axis < dim(); axis++) {
        size *= p_shaped(p_shape, axis);
    }
    return size;
}

fn index_range(global_id: vec3<u32>, num_workgroups: vec3<u32>) -> vec2<i32> {
    // todo: use all 3 workgroup dimensions.
    let id = global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) + global_id.x;
    let start_index = i32(id * p_chunk_size());
    let end_index = start_index + i32(p_chunk_size());
    return vec2<i32>(start_index, end_index);
}
