module App.Solutions.Day03

open Microsoft.FSharp.Collections

open App.Helpers
open type FSharp.UMX.UMX

[<Measure>] type r;
[<Measure>] type c;

// column, row
type Coord = int<c> * int<r>
type PartNumber =
    { Coords : Coord list
      Value : int }
type Symbol =
    { Coord : Coord
      Value : char } 
type Board =
    { PartNumbers : PartNumber list
      Symbols : Symbol list
      Dims : int<c> * int<r> }

let deltas =
    [ -1<c>, -1<r>
      0<c>, -1<r>
      1<c>, -1<r>
      -1<c>, 0<r>
      1<c>, 0<r>
      -1<c>, 1<r>
      0<c>, 1<r>
      1<c>, 1<r> ]

let getAdjacentCoords ((c, r) : Coord) =
    deltas
    |> List.map (fun (dc, dr) -> (dc + c, dr + r))

let pruneExternalCoords ((columnCount, rowCount) : int<c> * int<r>) (coords : Coord list) =
    coords
    |> List.filter (fun (c, r) ->
           c >= 0<c> && c < columnCount
        && r >= 0<r> && r < rowCount)

let getBoardDims (ss : string list) : int<c> * int<r> =
    let columnCount = ss |> List.head |> (fun s -> s.Length)
    let rowCount = ss.Length
    (columnCount |> tag, rowCount |> tag)

let (|Symbol|Space|) c =
    match c with
    | '.' -> Space
    | _ -> Symbol

let (|PartNumberDigit|Other|) c =
    match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> PartNumberDigit
    | _ -> Other

let parseBoard ss =
    let columnCount, rowCount = getBoardDims ss
    let totalCellCount = (columnCount |> untag) * (rowCount |> untag)
    
    let rec loop board i acc =
        if i >= totalCellCount then
            board
        else
            let r = i / (columnCount |> untag)
            let c = i % (columnCount |> untag)
            match ss[r][c] with
            | PartNumberDigit -> loop board (i + 1) ((c |> tag, r |> tag)::acc)
            | Other ->
                // flush acc
                let board =
                    if acc |> List.isEmpty |> not then
                        let value =
                            acc
                            |> List.rev
                            |> List.map (fun (c, r) -> ss[r |> untag][c |> untag])
                            |> System.String.Concat
                            |> int
                        { board with PartNumbers = { Coords = acc; Value = value }::board.PartNumbers }
                    else
                        board
                        
                match ss[r][c] with
                | Symbol ->
                    let value = ss[r |> untag][c |> untag]
                    let symbol =
                        { Coord = (c |> tag, r |> tag)
                          Value = value }
                    loop { board with Symbols = symbol::board.Symbols} (i + 1) []
                | Space -> loop board (i + 1) []
    
    loop { PartNumbers = []; Symbols = []; Dims = columnCount, rowCount } 0 []

let areTouching boardDims partNumber symbol =
    partNumber.Coords
    |> List.collect getAdjacentCoords
    |> List.distinct
    |> pruneExternalCoords boardDims
    |> List.exists (fun c -> c = symbol.Coord)

let prunePartNumbersNonAdjacentToSymbol board =
    { board with
        PartNumbers =
           board.PartNumbers
           |> List.filter (fun pn ->
               board.Symbols
               |> List.exists (areTouching board.Dims pn)) }

let getGearRatios board =
    board.Symbols
    |> List.filter (fun s ->
        match s.Value with
        | '*' -> true
        | _ -> false)
    |> List.choose (fun s ->
        let touchingPartNumbers =
            board.PartNumbers
            |> List.filter (fun pn -> areTouching board.Dims pn s)
        match touchingPartNumbers with
        | [ a; b ] -> Some (a.Value * b.Value)
        |_ -> None)
    |> List.sum

let calculateResult_01 ss =
    ss
    |> parseBoard
    |> prunePartNumbersNonAdjacentToSymbol
    |> (fun (b : Board) -> b.PartNumbers |> List.sumBy (fun pn -> pn.Value))

let calculateResult_02 ss =
    ss
    |> parseBoard
    |> prunePartNumbersNonAdjacentToSymbol
    |> getGearRatios

// wrappers
let solve_puzzle_1 () =
    
    File.readLines @"./inputs/day03.txt"
    |> Seq.toList
    |> calculateResult_01
    |> printfn "Day03, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day03.txt"
    |> Seq.toList
    |> calculateResult_02
    |> printfn "Day03, puzzle 2 -> result = %A"