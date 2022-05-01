module Parser.Tests

open System
open BindingGenerator.Parsing
open BindingGenerator.Parsing.Ast
open BindingGenerator.Parsing.Parsers
open FParsec
open Expecto

let run<'a> (parser : Parser<'a, _>) str =
    runParserOnString parser () "parser" str
    |> function
        | Success (result, _, _) -> result
        | Failure (str, err, _) -> failwith $"({str}){err}"

let test parse text expected =
    testCase $"{expected} = {text}"
    <| fun _ ->
        let result = run parse text
        Expect.equal result expected $"{result} <> {expected}"

let preprocessorTests =
    let test = test PreProcessorDirective.parse

    let danglingParen =
        """#ifdef __cplusplus
} // extern "C"
#endif"""

    let externBlock =
        """#ifdef __cplusplus
extern "C" {
#endif"""

    testList
        "Pre-Processor Directives"
        [
            QuoteInclude "wgpu.h"
            |> test "#include \"wgpu.h\""
            QuoteInclude "wgpu.h"
            |> test "#include \"wgpu.h\"\r\n"
            BracketInclude "stdint.h"
            |> test "#include <stdint.h>"
            Ignored "ifdef __cplusplus"
            |> test "#ifdef __cplusplus"
            Ignored "endif" |> test danglingParen
            Ignored "endif" |> test externBlock
            Comment "hello" |> test "//hello"
            Comment " hello " |> test "/* hello */"

        ]

let primitiveTests =
    let test = test PrimitiveType.parse

    testList
        "Primitive Types"
        [
            Char |> test "char"
            Str |> test "char const *"
            Str |> test "const char *"
            I32 |> test "int32_t"
            U32 |> test "uint32_t"
            I64 |> test "int64_t"
            U64 |> test "uint64_t"
            Bool |> test "bool"
            Float |> test "float"
            Double |> test "double"
            Void |> test "void"
        ]

let cTypeTests =
    let test = test CType.parse

    testList
        "C Types"
        [
            Identifier "WGPUAdapterExtras"
            |> test "WGPUAdapterExtras"
            Identifier "wgpuDevicePoll"
            |> test "wgpuDevicePoll"
            Float |> Primitive |> test "float"
            I64 |> Primitive |> test "int64_t"
            Void |> Primitive |> test "void"
            Identifier "WGPUInstanceDescriptor"
            |> ConstPointer
            |> test "WGPUInstanceDescriptor const * "
            Void |> Primitive |> Pointer |> test "void * "
            Void |> Primitive |> Pointer |> test "void* "
        ]

let enumTests =
    let test = test CEnum.parse

    let enumDef =
        """typedef enum WGPULogLevel {
    WGPULogLevel_Off = 0x00000000,
    WGPULogLevel_Error = 0x00000001,
    WGPULogLevel_Warn = 0x00000002,
    WGPULogLevel_Info = 0x00000003
} WGPULogLevel;
"""

    let flagDef =
        """typedef enum WGPUMapMode {
    WGPUMapMode_None = 0x00000000,
    WGPUMapMode_Read = 0x00000001
} WGPUMapMode;
typedef WGPUFlags WGPUMapModeFlags;
"""

    let enumExpected : CEnum =
        {
            Type = "WGPULogLevel"
            Branches =
                [
                    { Name = "WGPULogLevel_Off" ; Value = 0 }
                    { Name = "WGPULogLevel_Error" ; Value = 1 }
                    { Name = "WGPULogLevel_Warn" ; Value = 2 }
                    { Name = "WGPULogLevel_Info" ; Value = 3 }
                ]
            IsFlag = false
        }

    let flagExpected : CEnum =
        {
            Type = "WGPUMapMode"
            Branches =
                [
                    { Name = "WGPUMapMode_None" ; Value = 0 }
                    { Name = "WGPUMapMode_Read" ; Value = 1 }
                ]
            IsFlag = true
        }

    testList "Enum tests" [ enumExpected |> test enumDef ; flagExpected |> test flagDef ]

let structTests =
    let test = test Struct.parse

    let structDefinition1 =
        """typedef struct WGPUExtent3D {
    uint32_t width;
    uint32_t height;
    uint32_t depthOrArrayLayers;
} WGPUExtent3D;
"""

    let structExpected1 =
        {
            Type = "WGPUExtent3D"
            Fields =
                [
                    { Name = "width" ; Type = Primitive U32 }
                    { Name = "height" ; Type = Primitive U32 }
                    { Name = "depthOrArrayLayers" ; Type = Primitive U32 }
                ]
        }

    let structDefinition2 =
        """typedef struct WGPUSurfaceDescriptorFromMetalLayer {
    WGPUChainedStruct chain;
    void * layer;
} WGPUSurfaceDescriptorFromMetalLayer;
"""

    let structExpected2 =
        {
            Type = "WGPUSurfaceDescriptorFromMetalLayer"
            Fields =
                [
                    { Name = "chain" ; Type = Identifier "WGPUChainedStruct" }
                    { Name = "layer" ; Type = Pointer <| Primitive Void }
                ]
        }

    testList
        "Struct tests"
        [ structExpected1 |> test structDefinition1 ; structExpected2 |> test structDefinition2 ]

let pointerHandleTests =
    let test = test PointerHandle.parse

    let handle = "typedef struct WGPUAdapterImpl* WGPUAdapter;"

    let handleExpected : PointerHandle =
        { Type = "WGPUAdapter" ; PointedType = Identifier "WGPUAdapterImpl" }

    testList "Pointer Handle Tests" [ handleExpected |> test handle ]

let functionPointerTests =
    let test = test FunctionPointer.parse

    let callback =
        "typedef void (*WGPULogCallback)(WGPULogLevel level, const char *msg);"

    let callbackExpected : FunctionPointer =
        {
            Type = "WGPULogCallback"
            ReturnType = Primitive Void
            Parameters =
                [
                    { Type = Identifier "WGPULogLevel" ; Name = "level" }
                    { Type = Primitive Str ; Name = "msg" }
                ]
        }

    testList "Function Pointer Tests" [ callbackExpected |> test callback ]

let functionTests =
    let test = test Function.parse

    let free = "void wgpuSetLogLevel(WGPULogLevel level);"

    let freeExpected =
        {
            Type = "wgpuSetLogLevel"
            ReturnType = Primitive Void
            Parameters = [ { Name = "level" ; Type = Identifier "WGPULogLevel" } ]

        }

    let wgpuExport =
        "WGPU_EXPORT void wgpuRenderBundleEncoderPopDebugGroup(WGPURenderBundleEncoder renderBundleEncoder);"

    let wgpuExportExpected =
        {
            Type = "wgpuRenderBundleEncoderPopDebugGroup"
            ReturnType = Primitive Void
            Parameters =
                [ { Name = "renderBundleEncoder" ; Type = Identifier "WGPURenderBundleEncoder" } ]

        }

    let explicitVoid = "uint32_t wgpuGetVersion(void);"

    let explicitVoidExpected =
        {
            Type = "wgpuGetVersion"
            ReturnType = Primitive U32
            Parameters = []

        }

    testList
        "Function Tests"
        [
            freeExpected |> test free
            wgpuExportExpected |> test wgpuExport
            explicitVoidExpected |> test explicitVoid
        ]

let headerFileTests =
    testCase "Parses Header"
    <| fun _ ->
        let result = Headers.parse ()
        Expect.isOk result "Failed to parse combined header"

[<Tests>]
let tests =
    testList
        "Header Parsing"
        [
            preprocessorTests
            primitiveTests
            cTypeTests
            enumTests
            structTests
            pointerHandleTests
            functionPointerTests
            functionTests
            headerFileTests
        ]
