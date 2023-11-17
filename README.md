# wgpu-tensors - Tensors with wgpu 🔥

**Work In Progress**

A tensor crate using [wgpu][1].


## TODO

 - [x] PRNG using hybrid tausworthe/LCG ([2])
 - [ ] use parallel prefix for reduce operations ([3])
 - [ ] matrix multiplication
 - [x] GGUF loading and metadata serde integration ([gguf spec][4], [tensor format][5])


[1]: https://github.com/gfx-rs/wgpu
[2]: https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-37-efficient-random-number-generation-and-application
[3]: https://developer.nvidia.com/gpugems/gpugems3/part-vi-gpu-computing/chapter-39-parallel-prefix-sum-scan-cuda
[4]: https://github.com/ggerganov/ggml/blob/master/docs/gguf.md
[5]: https://cca.informatik.uni-freiburg.de/debugging/ws23/FORMAT.html
