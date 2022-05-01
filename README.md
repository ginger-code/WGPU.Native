
# WGPU.Native

## Low-level .NET bindings for wgpu-native

*Buyer beware, these binding require heavy use of native pointers and marshaling*

This library is a set of generated WGPU.Native bindings for F# (C# code generation is planned, but you can use this library too!).

No shortcuts taken here- code is custom-generated based on a handwritten header parser, all developed in F#.

Documentation is a work in progress, and many aspects of the generation process and logic could use heavy refactoring, so contributions are welcome!

*wgpu-fs, a safe wrapper for WGPU.FSharp.Native which aims to bring wgpu-rs semantics to F#, is currently in development.*

>WebGPU is an API that exposes modern computer graphics capabilities, specifically Direct3D 12, Metal, and Vulkan, for performing rendering and computation operations on a graphics processing unit (GPU).
>
>wgpu is an implementation of WebGPU written in Rust
>
>wgpu-native is the native implementation of wgpu, based on wgpu-core

You may find the following links helpful:

* [wgpu-native examples](https://github.com/gfx-rs/wgpu-native/tree/master/examples)
* [wgpu docs](https://docs.rs/wgpu/latest/wgpu/index.html)
