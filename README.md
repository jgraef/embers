# 🔥 embers - Tensors with wgpu

**Work In Progress**

A tensor crate using [wgpu][1].


## TODO

 - [x] PRNG using hybrid tausworthe/LCG ([2])
 - [x] GGUF parsing and metadata serde integration ([gguf spec][4])
 - [ ] use parallel prefix for reduce operations ([3])
 - [ ] matrix multiplication
 - [ ] quantized tensors ([tensor format][5], [quantization][6])
 - [ ] use [encase][7]
 - [ ] composable kernels (something like [naga-oil][8], [minify wgsl][10])
 - [ ] compute graph ([petgraph][9])

[1]: https://github.com/gfx-rs/wgpu
[2]: https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-37-efficient-random-number-generation-and-application
[3]: https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-39-parallel-prefix-sum-scan-cuda
[4]: https://github.com/ggerganov/ggml/blob/master/docs/gguf.md
[5]: https://cca.informatik.uni-freiburg.de/debugging/ws23/FORMAT.html
[6]: https://github.com/ggerganov/llama.cpp/issues/1240
[7]: https://github.com/teoxoy/encase
[8]: https://github.com/bevyengine/naga_oil/
[9]: https://github.com/petgraph/petgraph
[10]: https://docs.rs/wgsl-minifier/latest/wgsl_minifier/fn.minify_module.html
