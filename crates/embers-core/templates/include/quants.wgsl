
struct block_q8_0_t {
    d: f16,
    qs: array<u32, 8>, // [i8; 32]
}

fn q8_0_decode(b: ptr<block_q8_0_t, storage, read>, i: u32) -> f16 {
    return b.d * f16(b.qs[i]);
}
