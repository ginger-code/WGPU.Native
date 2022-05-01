# Binding Generator

This project is used to generate WGPU.FSharp.Native. *(WGPU.CSharp.Native planned)*

## Limitations

This parser doesn't rely on clang or any external toolchains, however it comes with limitations.

The generator is designed to work with wgpu headers, but will likely break on any other headers (and may break as wgpu is updated).

The parser isn't exhaustive, is hacky in places, and discards a lot of data that may become significant in later versions.



## How to generate
>Currently, only 64-bit binaries are present in the git repository.
>
>If you would like to make a 32-bit build, follow the instructions and choose 32-bit (`x86`) binary distributions.

1. Go to the [wgpu-native](https://github.com/gfx-rs/wgpu-native) release page for the most current release.
2. Download the following files, and copy and `dll`, `dylib`, `lib`, or `so` files to the `headers` directory.
    * wgpu-windows-x86_64-release.zip
    * wgpu-macos-x86_64-release.zip
    * wgpu-linux-x86_64-release.zip
3. Also grab the wgpu.h and webgpu.h files from the linux distribution, and place those into `headers`, as well
   * Overwrite the existing headers and binaries if needed
4. Make sure you have write permissions for src/WGPU.FSharp.Native
5. `dotnet build && cd bin/Debug/net6.0 && dotnet BindingGenerator.dll`
