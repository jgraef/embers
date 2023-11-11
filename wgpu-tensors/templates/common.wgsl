// parameter binding
@group(0)
@binding(0)
var<storage, read> parameters: array<i32>;

// tensor bindings
{% for binding in info.declaration.bindings %}
    @group(0)
    @binding({{ loop.index }})
    var<storage, {{ binding.read_write }}> {{ binding.name }}: array<{{ binding.ty }}>;
{% endfor %}

// constants

const WORKGROUP_SIZE_X = {{ info.work_group_size.x }}u;
const WORKGROUP_SIZE_Y = {{ info.work_group_size.y }}u;
const WORKGROUP_SIZE_Z = {{ info.work_group_size.z }}u;
const WORKGROUP_SIZE = {{ info.work_group_size.product() }}u;

// num bindings excluding the parameter binding
const NUM_BINDINGS = {{ info.declaration.bindings.len() }}u;


// parameters

fn dim() -> u32 {
    return u32(parameters[0u]);
}

fn p_count() -> u32 {
    return u32(parameters[1u]);
}

fn p_int(i: u32) -> i32 {
    return parameters[i + 2u];
}

fn p_shaped(i: u32, axis: u32) -> i32 {
    let p = u32(p_int(i)) + p_count() + 2u;
    return parameters[u32(p) + axis];
}

{% for parameter in info.declaration.parameters %}
    const P_{{ parameter.name|upper }}: u32 = {{ loop.index - 1 }}u;

    {% match parameter.ty %}
        {% when KernelParameterType::Int %}
            fn p_{{ parameter.name }}() -> i32 {
                return p_int({{ loop.index - 1 }}u);
            }
        {% when KernelParameterType::Shaped %}
            fn p_{{ parameter.name }}(axis: u32) -> i32 {
                return p_shaped({{ loop.index - 1 }}u, axis);
            }
        {% when KernelParameterType::Strider %}
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
