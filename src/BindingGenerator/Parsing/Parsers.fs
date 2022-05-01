module BindingGenerator.Parsing.Parsers

open System
open BindingGenerator.Parsing.Ast
open FParsec


let pConst : Parser<string, unit> = pstring "const"
let pChar : Parser<string, unit> = pstring "char"
let pPtr : Parser<char, unit> = pchar '*'

let pTypedef : Parser<string, unit> = pstring "typedef"
let pEnum : Parser<string, unit> = pstring "enum"
let pStruct : Parser<string, unit> = pstring "struct"
let parenthesized x : Parser<'a, unit> = between (pchar '(') (pchar ')') x
let bracketed x : Parser<'a, unit> = between (pchar '{' >>. spaces) (spaces >>. pchar '}') x
let angleBracketed x : Parser<'a, unit> = between (pchar '<') (pchar '>') x
let quoted x : Parser<'a, unit> = between (pchar '"') (pchar '"') x
let pSemicolon : Parser<char, unit> = pchar ';'
let pExternStatement : Parser<string, unit> = pstring "extern" >>? restOfLine true
let pHash : Parser<char, unit> = pchar '#'
let commentLine : Parser<string, unit> = pstring "//" >>? restOfLine true
let silentCommentLine = spaces >>? commentLine >>? spaces
let spaces = silentCommentLine <|> spaces

let ignoredContent : Parser<_, _> =
    spaces
    <|> (pExternStatement |>> ignore)
    <|> (pSemicolon |>> ignore)
    <|> (pchar '}' >>? restOfLine true |>> ignore)

module PreProcessorDirective =
    let parse : Parser<PreProcessorDirective, unit> =
        let pInclude =
            let includeStatement = pHash >>? pstring "include" >>? spaces

            let pBracketInclude =
                includeStatement
                >>? angleBracketed (manyChars (noneOf ">"))
                |>> BracketInclude

            let pQuoteInclude =
                includeStatement
                >>? quoted (manyChars (noneOf "\""))
                |>> QuoteInclude

            pBracketInclude <|> pQuoteInclude

        let pComment = commentLine |>> Comment

        let blockComment =
            pstring "/*"
            >>. (manyCharsTill anyChar (pstring "*/"))
            .>> spaces
            |>> Comment

        let pIgnored = pHash >>? restOfLine true |>> Ignored

        let pPhantomBracket =
            pIgnored
            >>? (pstring "}" <|> pExternStatement)
            >>? spaces
            >>? pIgnored

        pInclude
        <|> pPhantomBracket
        <|> pIgnored
        <|> pComment
        <|> blockComment

module PrimitiveType =
    let parse : Parser<PrimitiveType, unit> =
        let pStr =
            pConst
            >>? spaces1
            >>? pChar
            >>? spaces
            >>? pstring "*"
            >>% Str

        let pStr2 =
            pChar
            >>? spaces1
            >>? pConst
            >>? spaces
            >>? pstring "*"
            >>% Str

        choice [
            pStr
            pStr2
            (pChar >>% Char)
            (stringReturn "uint16_t" U16)
            (stringReturn "uint32_t" U32)
            (stringReturn "uint64_t" U64)
            (stringReturn "size_t" U64)
            (stringReturn "int32_t" I32)
            (stringReturn "int64_t" I64)
            (stringReturn "bool" Bool)
            (stringReturn "float" Float)
            (stringReturn "double" Double)
            (stringReturn "void" Void)
        ]
        .>>? spaces

let nonIdentifier =
    notFollowedBy (letter <|> digit <|> pchar '_')
    <|> spaces1
    <|> (pchar '{' |>> ignore)
    <|> (pchar '(' |>> ignore)
    <|> (pchar ')' |>> ignore)
    <|> (pchar '}' |>> ignore)
    <|> (pSemicolon |>> ignore)
    <|> (pPtr |>> ignore)


let pIdentifier =
    let first = (letter <|> pchar '_')
    let second = (manyCharsTill (letter <|> digit <|> pchar '_') nonIdentifier)
    first .>>.? second |>> (fun (f, s) -> $"{f}{s}")

module CType =
    let parse : Parser<CType, unit> =
        let primitive = PrimitiveType.parse |>> Primitive
        let reference = pIdentifier |>> Identifier
        let atomic = primitive <|> reference

        let constPointer =
            (atomic
             .>>? spaces
             .>>? pConst
             .>>? spaces
             .>>? pPtr)
            <|> (atomic
                 .>>? spaces
                 .>>? pPtr
                 .>>? spaces
                 .>>? pConst)
            |>> ConstPointer

        let pointer = atomic .>>? spaces .>>? pPtr |>> Pointer

        choice [
            constPointer
            pointer
            primitive
            reference
        ]
        .>>? spaces

module CEnum =
    let parse : Parser<CEnum, unit> =
        let enumType =
            pTypedef
            >>? spaces
            >>? pEnum
            >>? spaces
            >>? pIdentifier
            .>>? spaces

        let enumEnd =
            pchar '}' >>? spaces >>? CType.parse
            .>>? spaces
            .>>? pSemicolon
            .>>? spaces

        let pWgpuFlags = pstring "WGPUFlags" .>>? spaces

        let enumFlags =
            (pTypedef
             >>? spaces
             >>? pWgpuFlags
             >>? spaces
             >>? CType.parse
             .>>? spaces
             .>>? pSemicolon
             .>>? spaces
             >>% true)
            <|>% false
            .>> spaces

        let enumBranch : Parser<EnumBranch, unit> =
            spaces >>. pIdentifier
            .>> spaces
            .>> pchar '='
            .>> spaces
            .>>. pint32
            .>> spaces
            |>> fun (name, value) -> { Name = name ; Value = value }

        let branches = between (pchar '{') enumEnd (sepBy enumBranch (pchar ','))

        tuple3 enumType branches enumFlags
        |>> fun (name, branches, isFlag) ->
                {
                    Type = name
                    Branches = branches
                    IsFlag = isFlag
                }

module Struct =
    let parse : Parser<Struct, unit> =
        let structType =
            pTypedef
            >>? spaces
            >>? pStruct
            >>? spaces
            >>? pIdentifier
            .>>? spaces
            .>>? pchar '{'
            .>>? spaces

        let structEnd =
            pchar '}' >>? spaces >>? CType.parse
            .>>? spaces
            .>>? pSemicolon
            .>> spaces

        let field : Parser<StructField, unit> =
            spaces
            >>. optional (pStruct >>. spaces)
            >>. CType.parse
            .>> spaces
            .>>. pIdentifier
            .>> spaces
            .>> pSemicolon
            .>> spaces
            |>> fun (cType, name) -> { Name = name ; Type = cType }

        let fields = manyTill field structEnd

        structType .>>. fields
        |>> fun (cType, fields) -> { Type = cType ; Fields = fields }

module PointerHandle =
    let parse : Parser<PointerHandle, unit> =
        spaces
        >>? pTypedef
        >>? spaces
        >>? pStruct
        >>? spaces
        >>? (pIdentifier |>> Identifier)
        .>>? spaces
        .>>? pPtr
        .>>? spaces
        .>>.? pIdentifier
        .>>? spaces
        .>>? pSemicolon
        .>> spaces
        |>> fun (pointed, cType) -> { PointedType = pointed ; Type = cType }

module FunctionParameter =
    let private parse : Parser<FunctionParameter, unit> =
        spaces >>. CType.parse .>> spaces .>>. pIdentifier
        .>> spaces
        |>> fun (cType, name) -> { Name = name ; Type = cType }

    let parseList =

        (parenthesized (
            ((pstring "void") |>> fun _ -> [])
            <|> (sepBy parse (spaces >>? pchar ',' >>? spaces))
         )
         .>> spaces)

module FunctionPointer =
    let parse : Parser<FunctionPointer, unit> =
        let name = parenthesized (pPtr >>. spaces >>. pIdentifier)

        (pTypedef >>? spaces >>? CType.parse .>>? spaces
         .>>.? name)
        .>>? spaces
        .>>.? FunctionParameter.parseList
        .>>? spaces
        .>>? pSemicolon
        .>>? spaces
        |>> fun ((returnType, cType), parameters) ->
                {
                    Type = cType
                    ReturnType = returnType
                    Parameters = parameters
                }

let pWgpuExport : Parser<unit, unit> = spaces >>? pstring "WGPU_EXPORT" >>. spaces

module Function =
    let parse : Parser<Function, unit> =
        optional pWgpuExport
        >>? (CType.parse .>> spaces .>>. pIdentifier)
        .>>? spaces
        .>>.? FunctionParameter.parseList
        .>>? spaces
        .>>? pSemicolon
        .>> spaces
        |>> fun ((returnType, cType), parameters) ->
                {
                    Type = cType
                    ReturnType = returnType
                    Parameters = parameters
                }

module HeaderDefinition =
    let parse : Parser<HeaderDefinition, unit> =
        choice [
            PointerHandle.parse |>> PointerHandleDefinition
            Struct.parse |>> StructDefinition
            CEnum.parse |>> EnumDefinition
            FunctionPointer.parse
            |>> FunctionPointerDefinition
            Function.parse |>> FunctionDefinition
        ]

module Ast =
    let parseNode : Parser<Ast, unit> =
        choice [
            HeaderDefinition.parse |>> Definition
            PreProcessorDirective.parse |>> Directive
        ]

    let parseAll = manyTill (spaces >>. parseNode .>> spaces) eof

let runParser (parser : Parser<'a, unit>) input : Result<'a, ParserError> =
    runParserOnString parser () "parser" input
    |> function
        | Failure (message, error, _) ->
            printfn $"Parser encountered an error{Environment.NewLine}{message}"
            Result.Error error

        | Success (result, _, _) -> Result.Ok result
