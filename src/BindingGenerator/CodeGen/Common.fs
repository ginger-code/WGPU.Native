module BindingGenerator.CodeGen.Common

open System.IO

[<Literal>]
let libPath = "../../../../WGPU.FSharp.Native/"
let copyLibs () =
    ["libwgpu.dll";"libwgpu.dylib";"libwgpu.lib";"libwgpu.so"]
    |> List.map (fun f -> $"../../../headers/{f}", $"{libPath}{f}",true)
    |> List.iter File.Copy
