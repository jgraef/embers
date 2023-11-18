// include/prng.wgsl

struct prng_t {
    z: array<u32, 4>,
    bool_buffer: u32,
    bool_buffer_i: u32,
    uniform_buffer: f32,
    uniform_buffer_i: bool,
}

fn prng_new(seed: array<u32, 4>) -> prng_t {
    return prng_t(seed, 0u, 0u, 0., false);
}

fn prng_from_param_and_global_id(p_seed: u32, global_id: vec3<u32>) -> prng_t {
    var seed = array<u32, 4>(
        u32(p_array(p_seed, 0u)),
        u32(p_array(p_seed, 1u)),
        u32(p_array(p_seed, 2u)),
        u32(p_array(p_seed, 3u)),
    );

    seed[1] += global_id.x * 1000000007u;
    seed[2] += global_id.y * 1000000007u;
    seed[3] += global_id.z * 1000000007u;

    return prng_new(seed);
}

fn prng_generate_u32(state: ptr<function, prng_t>) -> u32 {
    /*
    unsigned z1, z2, z3, z4; float HybridTaus() {   // Combined period is lcm(p1,p2,p3,p4)~ 2^121
        return 2.3283064365387e-10 * (              // Periods
          TausStep(z1, 13, 19, 12, 4294967294UL) ^  // p1=2^31-1
          TausStep(z2, 2, 25, 4, 4294967288UL) ^    // p2=2^30-1
          TausStep(z3, 3, 11, 17, 4294967280UL) ^   // p3=2^28-1
          LCGStep(z4, 1664525, 1013904223UL)        // p4=2^32
        );
    }
    */

    (*state).z[0] = _prng_taus_step((*state).z[0], 13u, 19u, 12u, 4294967294u);
    (*state).z[1] = _prng_taus_step((*state).z[1], 2u, 25u, 4u, 4294967288u);
    (*state).z[2] = _prng_taus_step((*state).z[2], 3u, 11u, 17u, 4294967280u);
    (*state).z[3] = _prng_lcg_step((*state).z[3], 1664525u, 1013904223u);

    return (*state).z[0] ^ (*state).z[1] ^ (*state).z[2] ^ (*state).z[3];
}

fn _prng_taus_step(z: u32, s1: u32, s2: u32, s3: u32, m: u32) -> u32 {
    /*
    // S1, S2, S3, and M are all constants, and z is part of the
    // private per-thread generator state.
    unsigned TausStep(unsigned &z, int S1, int S2, int S3, unsigned M) {
        unsigned b=(((z << S1) ^ z) >> S2);
        return z = (((z & M) << S3) ^ b);
    }
    */
    let b = ((z << s1) ^ z) >> s2;
    return ((z & m) << s3) ^ b;
}

fn _prng_lcg_step(z: u32, a: u32, c: u32) -> u32 {
    /*
    // A and C are constants
    unsigned LCGStep(unsigned &z, unsigned A, unsigned C) {
        return z=(A*z+C);
    }
    */
    return a * z + c;
}

fn prng_generate_i32(state: ptr<function, prng_t>) -> i32 {
    return i32(prng_generate_u32(state));
}

fn prng_generate_f32_uniform(state: ptr<function, prng_t>) -> f32 {
    // this constant is 1 / 2^32, so the resulting float is in [0, 1)
    return 2.3283064365387e-10 * f32(prng_generate_u32(state));
}

fn prng_generate_f32_normal(state: ptr<function, prng_t>) -> f32 {
    if (*state).uniform_buffer_i {
        (*state).uniform_buffer_i = false;
        return (*state).uniform_buffer;
    }
    else {
        let u = vec2<f32>(prng_generate_f32_uniform(state), prng_generate_f32_uniform(state));
        let r = _box_muller_transform(u);
        (*state).uniform_buffer = r.y;
        (*state).uniform_buffer_i = true;
        return r.x;
    }
}

fn _box_muller_transform(u: vec2<f32>) -> vec2<f32> {
    /*
    float2 BoxMuller() {
       float u0=HybridTaus (), u1=HybridTaus ();
       float r=sqrt(-2 log(u0));
       float theta=2*PI*u1;
       return make_float2(r*sin(theta),r*cos(theta));
    }
    */
    let r = sqrt(-2. * log(u.x));
    let theta = 2. * PI * u.y;
    return vec2<f32>(r * sin(theta), r * cos(theta));
}

fn prng_generate_bool(state: ptr<function, prng_t>) -> bool {
    if (*state).bool_buffer_i == 0u {
        (*state).bool_buffer = prng_generate_u32(state);
    }

    let value = ((*state).bool_buffer & 1u) != 0u;
    (*state).bool_buffer >>= 1u;
    (*state).bool_buffer_i += 1u;
    if (*state).bool_buffer_i == 32u {
        (*state).bool_buffer_i = 0u;
    }

    return value;
}
