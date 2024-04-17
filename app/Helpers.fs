[<AutoOpen>]
module App.Helpers

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Dictionary =
    let tryFind key (dict : Dictionary<_, _>) =
        match dict.TryGetValue(key) with
        | false, _ -> None
        | true, v -> Some v

module File =
    let readAllLines filePath = File.ReadAllLines(filePath)
    let readLines filePath = File.ReadLines(filePath)
    let readAllText filePath = File.ReadAllText(filePath)

module Int =
    let parseBinary b = Convert.ToInt32(b, 2)

module Option =
    let ofTryParse parser text =
        let success, value = parser text
        if success then Some value else None

module Seq =
    let toCouples xss =
        xss
        |> Seq.map
            (fun xs ->
                let a = Seq.item 0 xs
                let b = Seq.item 1 xs
                a, b)

    let toTriplets xss =
        xss
        |> Seq.map
            (fun xs ->
                let a = Seq.item 0 xs
                let b = Seq.item 1 xs
                let c = Seq.item 2 xs
                a, b, c)
    
    let mapAsCouples mapper xss =
        xss
        |> toCouples
        |> Seq.map (fun (a, b) -> mapper a b)
    
    let mapAsTriplets mapper xss =
        xss
        |> toTriplets
        |> Seq.map (fun (a, b, c) -> mapper a b c)

module Set =
    let exactlyOne xss =
        xss
        |> Set.toSeq
        |> Seq.exactlyOne

module String =
    let join (sep : string) (seq : string list) =
        String.Join(sep, seq)
    
    let replace (oldText : string) (newText : string) (str : string)  =
        str.Replace(oldText, newText)
    
    let split (del : string) (opts : StringSplitOptions) (str : string) =
        str.Split(del, opts)

module Match =
    let getValue (name : string) (m : Match) =
        m.Groups.[name].Value
   