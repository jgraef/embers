// include/parameters.wgsl

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

fn p_array(i: u32, j: u32) -> i32 {
    let p = u32(p_int(i)) + P_COUNT + 2u;
    return parameters[p + j];
}

{% for parameter in info.declaration.parameters %}
    const P_{{ parameter.name|upper }}: u32 = {{ loop.index - 1 }}u;

    {% match parameter.ty %}
        {% when crate::kernel::binding::KernelParameterType::Int %}
            fn p_{{ parameter.name }}() -> i32 {
                return p_int({{ loop.index - 1 }}u);
            }
        {% when crate::kernel::binding::KernelParameterType::Array %}
            fn p_{{ parameter.name }}(j: u32) -> i32 {
                return p_array({{ loop.index - 1 }}u, j);
            }
    {% endmatch %}
{% endfor %}
