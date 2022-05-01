module BindingGenerator.CodeGen.FSharp

open System
open System.IO
open BindingGenerator.Parsing
open BindingGenerator.Parsing.Ast
open BindingGenerator.Parsing.Parsers

let private indent x = String.replicate x "    "
let private indentAnd x = indent x |> (+)

module Transforms =
    let renameFSharpKeywords =
        function
        | "type" -> "``type``"
        | "module" -> "``module``"
        | x -> x

    let dropFlagsFromType (t : string) = t.Replace("Flags", "")
    let dropWGPU (t : string) = if t.StartsWith "WGPU" then t.[4..] else t

    let formatType =
        renameFSharpKeywords
        >> dropFlagsFromType
        >> dropWGPU

module PrimitiveType =
    let generate =
        function
        | Char -> "char"
        | Str -> "string"
        | I32 -> "int32"
        | I64 -> "int64"
        | U16 -> "uint16"
        | U32 -> "uint32"
        | U64 -> "uint64"
        | Bool -> "bool"
        | Float -> "float"
        | Double -> "double"
        | Void -> "unit"

    let marshalAttributeUnbound =
        function
        | Str -> "MarshalAs(UnmanagedType.LPStr)"
        | Bool -> "MarshalAs(UnmanagedType.U1)"
        | _ -> ""

    let marshalAttribute primitive =
        match marshalAttributeUnbound primitive with
        | "" -> ""
        | x -> $"[<{x}>]"

module CType =
    let rec generate =
        function
        | Identifier id -> id |> Transforms.formatType
        | Primitive primitive -> PrimitiveType.generate primitive
        | Pointer (Primitive Void) -> "nativeint"
        | ConstPointer (Primitive Void) -> "nativeint"
        | Pointer _ -> $"nativeint"
        | ConstPointer _ -> $"nativeint"

    let rec generateParam =
        function
        | Identifier id -> id |> Transforms.formatType
        | Primitive primitive -> PrimitiveType.generate primitive
        | Pointer (Primitive Void) -> "nativeint"
        | ConstPointer (Primitive Void) -> "nativeint"
        | Pointer cType -> $"{generate cType} byref"
        | ConstPointer cType -> $"{generate cType} inref"



    let rec generateExtern =
        function
        | Identifier id -> id |> Transforms.formatType
        | Primitive primitive -> PrimitiveType.generate primitive
        | Pointer (Primitive Void) -> "nativeint"
        | ConstPointer (Primitive Void) -> "nativeint"
        | Pointer cType -> $"{generate cType}"
        | ConstPointer cType -> $"{generate cType}"

    let marshalAttributeUnbound =
        function
        | Primitive primitive -> PrimitiveType.marshalAttributeUnbound primitive
        | _ -> ""

    let marshalAttribute =
        function
        | Primitive primitive -> PrimitiveType.marshalAttribute primitive
        | _ -> ""

    let externAttributes =
        function
        | ConstPointer cType when cType <> Primitive Void ->
            let marshal = marshalAttributeUnbound cType
            if marshal = "" then "[<In>]" else $"[<In ; {marshal}>]"
        | Pointer cType when cType <> Primitive Void ->
            let marshal = marshalAttributeUnbound cType

            if marshal = "" then
                "[<In ; Out>]"
            else
                $"[<In ; Out ; {marshal}>]"
        | Primitive primitive -> PrimitiveType.marshalAttribute primitive
        | _ -> ""


    let isManaged =
        function
        | Primitive Str -> true
        | _ -> false

    let isManagedType (parsed : HeaderDefinition list) =
        let structIsManaged (id : Identifier) =
            parsed
            |> List.tryFind (
                function
                | StructDefinition x when x.Type = id -> true
                | _ -> false
            )
            |> function
                | Some (StructDefinition s) ->
                    s.Fields
                    |> List.exists (fun f -> isManaged f.Type)
                | _ -> false

        function
        | Identifier id -> structIsManaged id
        | Pointer (Identifier id) -> structIsManaged id
        | ConstPointer (Identifier id) -> structIsManaged id
        | _ -> false

    let rec generateParamByref (parsed : HeaderDefinition list) =
        function
        | Identifier id -> id |> Transforms.formatType
        | Primitive primitive -> PrimitiveType.generate primitive
        | Pointer (Primitive Void) -> "nativeint"
        | ConstPointer (Primitive Void) -> "nativeint"
        | Pointer cType ->
            if isManagedType parsed cType then
                $"{generate cType} byref"
            else
                $"{generate cType} nativeptr"
        | ConstPointer cType ->
            if isManagedType parsed cType then
                $"{generate cType} byref"
            else
                $"{generate cType} nativeptr"

    let getPointerSymbol (parsed : HeaderDefinition list) =
        function
        | ConstPointer x when x <> Primitive Void -> if isManagedType parsed x then "&" else "*"
        | Pointer x when x <> Primitive Void -> if isManagedType parsed x then "&" else "*"
        | _ -> ""

module CEnum =
    let generate (enum : CEnum) =
        let formatBranchName (branch : EnumBranch) =
            branch
                .Name
                .Replace($"{enum.Type}_", "")
                .Replace("SType_", "")
            |> function
                | "1D" -> "OneDimensional"
                | "2D" -> "TwoDimensional"
                | "2DArray" -> "TwoDimensionalArray"
                | "3D" -> "ThreeDimensional"
                | x -> x
            |> Transforms.formatType

        let formatBranch (branch : EnumBranch) =
            indentAnd 1 $"""| {branch |> formatBranchName} = %i{branch.Value}"""

        let branches =
            List.map formatBranch
            >> List.filter (fun b -> not <| b.StartsWith(indentAnd 1 "| Force32"))
            >> String.concat Environment.NewLine

        let flags =
            if enum.IsFlag then
                $"[<Flags>]{Environment.NewLine}"
            else
                ""

        $"""{flags}type {enum.Type |> Transforms.formatType} =
{branches enum.Branches}
"""


module Struct =
    let generate (def : Struct) =
        let name = def.Type |> Transforms.formatType
        let isManaged = CType.isManagedType [StructDefinition def ] <| Identifier def.Type
        let managedComment =
            if isManaged then
                "///(Managed type)"
            else
                "///(Unmanaged type)"

        let formatField (field : StructField) =
            let attr =
                match CType.marshalAttribute field.Type with
                | "" -> ""
                | x -> $"{x}{Environment.NewLine}{indent 2}"

            match field.Type with
            | Pointer x ->
                let id = CType.generate x
                indentAnd 2 $"///{id} byref{Environment.NewLine}"
            | ConstPointer x ->
                let id = CType.generate x
                indentAnd 2 $"///{id} inref{Environment.NewLine}"
            | _ -> ""
            + indentAnd
                2
                $"""{attr}val {field.Name
                               |> CaseExtensions.StringExtensions.ToPascalCase
                               |> Transforms.formatType} : {CType.generate field.Type}"""

        let formatParam (field : StructField) =
            $"""?{field.Name |> Transforms.formatType} : {CType.generate field.Type}"""

        let formatInit (field : StructField) =
            let defaultFunc =
                match field.Type with
                | Pointer _ -> $"""Option.defaultValue IntPtr.Zero"""
                | ConstPointer _ -> $"""Option.defaultValue IntPtr.Zero"""
                | _ -> $"""Option.defaultValue Unchecked.defaultof<{CType.generate field.Type}>"""

            $"""{field.Name
                 |> CaseExtensions.StringExtensions.ToPascalCase
                 |> Transforms.formatType} = {defaultFunc} {field.Name |> Transforms.formatType}"""

        let fields =
            def.Fields
            |> List.map formatField
            |> function
                | [] -> ""
                | x -> x |> String.concat Environment.NewLine

        let constructorParams =
            "("
            + (def.Fields
               |> List.map formatParam
               |> function
                   | [] -> ""
                   | x -> x |> String.concat ", ")
            + ")"

        let constructorBody =
            "{"
            + (def.Fields
               |> List.map formatInit
               |> function
                   | [] -> ""
                   | x -> x |> String.concat " ; ")
            + "}"

        let constructor = indentAnd 2 $"""new {constructorParams} = {constructorBody}"""

        $"""{managedComment}
[<StructuralEquality ;
  StructuralComparison ;
  StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)>]
type {name} =
{indent 1}struct
{fields}
{constructor}
{indent 1}end
"""

module PointerHandle =
    open Documentation.StructDocs

    let generate (handle : PointerHandle) (parsed : HeaderDefinition list) =
        let name = handle.Type |> Transforms.formatType
        let fNamePrefix = $"wgpu{name}"
        let doc = findStructDocs name
        let description = Documentation.formatDocDescription 0 doc.Description

        let memberFuncs =
            parsed
            |> List.choose (
                function
                | FunctionDefinition fd when fd.Type.StartsWith fNamePrefix ->
                    match fd.Parameters with
                    | x :: _xs when x.Type = Identifier handle.Type -> Some fd
                    | _ -> None
                | _ -> None
            )

        let formatFunc (f : Function) =
            let fName = f.Type.Replace(fNamePrefix, "")
            let fDoc = getStructFunction doc fName

            let description =
                (fDoc.Description
                 |> Option.map (Documentation.formatDocDescription 1)
                 |> Option.map (fun x -> x + Environment.NewLine)
                 |> Option.defaultValue "")

            let p g = f.Parameters |> List.skip 1 |> List.map g

            let memberParameters =
                let fMember (p : FunctionParameter) =
                    $"""{p.Name |> Transforms.formatType}: {p.Type
                                                            |> CType.generateParamByref parsed
                                                            |> Transforms.formatType}"""

                p fMember |> String.concat ", "

            let callParameters =
                "this"
                :: p (fun p ->
                    match p.Type with
                    | ConstPointer x when x <> Primitive Void ->
                        if CType.isManagedType parsed x then
                            $"&{p.Name}"
                        else
                            $"{p.Name}"
                    | Pointer x when x <> Primitive Void ->
                        if CType.isManagedType parsed x then
                            $"&{p.Name}"
                        else
                            $"{p.Name}"
                    | _ -> p.Name
                )
                |> String.concat ", "
                |> function
                    | x when x = " this" -> x
                    | x -> $"({x})"


            $"""{description}{indent 1}member this.{fName}({memberParameters}) = {f.Type}{callParameters}"""

        let funcs =
            memberFuncs
            |> List.map formatFunc
            |> String.concat Environment.NewLine

        $"""{description}
///(Unmanaged type)
[<StructuralEquality ;
  StructuralComparison ;
  StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)>]
type {handle.Type |> Transforms.formatType} =
{indent 1}struct
{indent 2}val handle : {CType.generate handle.PointedType}
{indent 2}new(ptr) = {{ handle = ptr }}
{indent 1}end
{funcs}
{indent 1}override this.ToString() = $"%%X{{this.handle}}"
and {CType.generate handle.PointedType} = nativeint
"""

module FunctionPointer =
    let generate (fp : FunctionPointer) =
        let formatFirstParam (p : FunctionParameter) =
            $"""{indent 2}{CType.marshalAttribute p.Type} {p.Name |> Transforms.formatType}:{CType.generateParam p.Type}"""

        let formatParam (p : FunctionParameter) =
            $"""{indent 2} * {CType.marshalAttribute p.Type} {p.Name |> Transforms.formatType}:{CType.generateParam p.Type}"""

        let paramList =
            match fp.Parameters with
            | [] -> [ indentAnd 2 "unit" ]
            | [ x ] -> [ formatFirstParam x ]
            | x :: xs ->
                (formatFirstParam x)
                :: (xs |> List.map formatParam)
            |> String.concat Environment.NewLine

        let formatType =
            function
            | "Proc" -> "Proc"
            | x when x.StartsWith("Proc") -> x.[4..]
            | x -> x

        $"""[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type {fp.Type |> Transforms.formatType |> formatType}
{indent 1}= delegate of
{paramList}
{indent 2}  -> {CType.generate fp.ReturnType}
"""

module Function =
    let generate (f : Function) (parsed : HeaderDefinition list) =
        let formatParam (p : FunctionParameter) =
            let refSymbol = CType.getPointerSymbol parsed p.Type
            $"""{CType.externAttributes p.Type} {CType.generateExtern p.Type}{refSymbol} {p.Name}"""

        let parameters =
            f.Parameters
            |> List.map formatParam
            |> function
                | [] -> ""
                | x -> x |> String.concat ", "

        let returnType =
            match f.ReturnType with
            | Primitive Void -> "void"
            | _ -> CType.generateExtern f.ReturnType

        $"""[<DllImport("libwgpu",
{indent 2}EntryPoint = "{f.Type |> Transforms.formatType}",
{indent 2}CallingConvention = CallingConvention.Cdecl)>]
extern {returnType} {f.Type}({parameters})
"""


module HeaderDefinition =
    let generate (parsed : HeaderDefinition list) =
        function
        | EnumDefinition enum -> CEnum.generate enum
        | StructDefinition str -> Struct.generate str
        | PointerHandleDefinition ph -> PointerHandle.generate ph parsed
        | FunctionPointerDefinition fp -> FunctionPointer.generate fp
        | FunctionDefinition f -> Function.generate f parsed

module Ast =
    let generate (parsed : Ast list) =
        function
        | Definition definition ->
            let parsed =
                parsed
                |> List.choose (
                    function
                    | Definition def -> Some def
                    | _ -> None
                )

            HeaderDefinition.generate parsed definition
        | _ -> ""

let moduleHeader =
    """//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------
module rec WGPU.Native

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.Core

"""

let parseAndGenerate () =
    match Headers.parse () with
    | Error parserError -> failwith $"{parserError}"
    | Ok parsed ->
        moduleHeader
        :: (parsed |> List.map (Ast.generate parsed))
        |> String.concat Environment.NewLine

let parseToFile () =
    let generated = parseAndGenerate ()
    File.WriteAllText("../../../../WGPU.FSharp.Native/WGPU.fs", generated)
