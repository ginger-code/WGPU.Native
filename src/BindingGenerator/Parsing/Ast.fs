module BindingGenerator.Parsing.Ast

type PreProcessorDirective =
    | QuoteInclude of string
    | BracketInclude of string
    | Comment of string
    | Ignored of string

type PrimitiveType =
    | Char
    | Str
    | I32
    | I64
    | U16
    | U32
    | U64
    | Bool
    | Float
    | Double
    | Void

type Identifier = string

type CType =
    | Identifier of Identifier
    | Pointer of CType
    | ConstPointer of CType
    | Primitive of PrimitiveType

type EnumBranch = { Name : string ; Value : int }

type CEnum =
    {
        ///Definition name
        Type : Identifier
        Branches : EnumBranch list
        IsFlag : bool
    }

type StructField = { Name : string ; Type : CType }

type Struct =
    {
        ///Definition name
        Type : Identifier
        Fields : StructField list
    }

type PointerHandle =
    {
        ///Definition name
        Type : Identifier
        PointedType : CType
    }

type FunctionParameter = { Name : string ; Type : CType }

type FunctionPointer =
    {
        ///Definition name
        Type : Identifier
        ReturnType : CType
        Parameters : FunctionParameter list
    }

type Function =
    {
        ///Definition name
        Type : Identifier
        ReturnType : CType
        Parameters : FunctionParameter list
    }

type HeaderDefinition =
    | EnumDefinition of CEnum
    | StructDefinition of Struct
    | PointerHandleDefinition of PointerHandle
    | FunctionPointerDefinition of FunctionPointer
    | FunctionDefinition of Function

type Ast =
    | Directive of PreProcessorDirective
    | Definition of HeaderDefinition
