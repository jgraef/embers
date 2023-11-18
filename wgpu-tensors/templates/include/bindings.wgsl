// include/bindings.wgsl

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
        let i = u32(index % B_{{ binding.name|upper }}_NUM_PACKED);
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
                let i = u32(index % B_{{ binding.name|upper }}_NUM_PACKED);
                let output = &_b_{{ binding.name }}_encoded[index / B_{{ binding.name|upper }}_NUM_PACKED];
                {{ binding.encoding.encode }}
            }
        {% else %}
    {% endmatch %}
{% endfor %}

// num bindings excluding the parameter binding
const B_COUNT = {{ info.declaration.bindings.len() }}u;
