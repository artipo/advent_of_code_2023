module App.Solutions.Day02

open Microsoft.FSharp.Collections
open System
open System.Text.RegularExpressions

open App.Helpers

let gameRegex = Regex(@"Game (?<id>\d+): (?<picks>.*)", RegexOptions.Multiline)
let diceRegex = Regex(@"(?<count>\d+) (?<dice>red|green|blue)", RegexOptions.Multiline)

type Cube =
    | Red
    | Green
    | Blue

type Pick = Map<Cube, uint>

type Game =
    { Id : uint
      Picks : Pick array }

let parseDice =
    function
    | "red" -> Red
    | "green" -> Green
    | "blue" -> Blue

let parseGame line : Game =
    let m = gameRegex.Match(line)
    let id = m |> Match.getValue "id" |> uint
    
    let picks : Pick array =
        m
        |> Match.getValue "picks"
        |> String.split ";" StringSplitOptions.TrimEntries
        |> Array.map (fun s ->
            s
            |> String.split "," StringSplitOptions.TrimEntries
            |> Array.map diceRegex.Match
            |> Array.map (fun m ->
                // tuple (dice * count)
                 m |> Match.getValue "dice" |> parseDice,
                 m |> Match.getValue "count" |> uint)
            |> Map.ofArray)

    { Id = id
      Picks = picks }

let extractMaxPickedDices (game : Game) =
    game.Picks
    |> Array.collect (fun p -> p |> Map.toArray)
    |> Array.groupBy fst
    |> Array.map (fun (c, p) -> c, p |> Array.map snd |> Array.max)
    |> Map.ofArray

let isGamePossibleWith (diceAvailability : Pick) game =
    let maxPickedDices =
        game
        |> extractMaxPickedDices
    
    diceAvailability
    |> Map.toArray
    |> Array.map (fun (dice, availableCount) ->
        match maxPickedDices |> Map.tryFind dice with
        | None -> false
        | Some maxPickedCount -> maxPickedCount <= availableCount)
    |> Array.reduce (&&)

let calculateResult_01 diceAvailability ss =
    ss
    |> List.map parseGame
    |> List.filter (isGamePossibleWith diceAvailability)
    |> List.sumBy (fun g -> g.Id)

let getGamePower game =
    game
    |> extractMaxPickedDices
    |> Map.toArray
    |> Array.map snd
    |> Array.reduce (*)

let calculateResult_02 ss =
    ss
    |> List.map parseGame
    |> List.map getGamePower
    |> List.sum

// wrappers
let solve_puzzle_1 () =
    let diceAvailability =
        [ Red, 12u
          Green, 13u
          Blue, 14u ]
        |> Map.ofList
    
    File.readLines @"./inputs/day02.txt"
    |> Seq.toList
    |> calculateResult_01 diceAvailability
    |> printfn "Day02, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day02.txt"
    |> Seq.toList
    |> calculateResult_02
    |> printfn "Day02, puzzle 2 -> result = %A"