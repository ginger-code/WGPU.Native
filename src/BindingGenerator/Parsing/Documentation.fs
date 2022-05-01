module BindingGenerator.Parsing.Documentation

open System
open System.IO
open FSharp.Data
open Ast

type DocProvider = JsonProvider<"headers/docs.json">
let docs = DocProvider.Load "headers/docs.json"

module StructDocs =
    let findStructDocs (structType : string) =
        docs.Handles
        |> Array.find (fun x -> x.Type = structType)

    let getStructFunction (handle : DocProvider.Handle) (name : string) =
        handle.Functions
        |> Array.find (fun x -> x.Name = name || x.Free = name)

let private indent x = String.replicate x "    "

let formatDocDescription indentLevel (s : string) =
    $"<summary>\n{s}\n</summary>".Split('\n')
    |> Seq.map (
        function
        | "" -> $"{indent indentLevel}/// <br/>"
        | s -> $"{indent indentLevel}/// {s.Trim()}"
    )
    |> String.concat Environment.NewLine
