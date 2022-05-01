module BindingGenerator.Parsing.Headers

open System.IO
let readWgpuSource () = File.ReadAllText "headers/wgpu.h"
let readWebGpuSource () = File.ReadAllText "headers/webgpu.h"

let includeWebGpu (wgpuSource : string) webGpuSource =
    wgpuSource.Replace("#include \"webgpu.h\"", webGpuSource)

let private stripEnumTypeDef (source : string) = source.Replace("typedef uint32_t WGPUFlags;", "")
let private stripCppDanglingBracket (source : string) = source.Replace("} // extern \"C\"", "")

let private getCombinedHeader () =
    includeWebGpu (readWgpuSource ()) (readWebGpuSource ())
    |> stripEnumTypeDef
    |> stripCppDanglingBracket

let private parseHeader header = Parsers.runParser Parsers.Ast.parseAll header

let private stripDirectives =
    List.filter (fun node ->
        match node with
        | Ast.Directive _ -> false
        | Ast.Definition _ -> true
    )

let parse =
    getCombinedHeader
    >> parseHeader
    >> Result.map stripDirectives
