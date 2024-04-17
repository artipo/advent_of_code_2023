module App.Solutions.Day04

open Microsoft.FSharp.Collections
open System
open System.Linq
open System.Text.RegularExpressions

open App.Helpers

let cardRegex = Regex(@"Card\s+(?<id>\d+):\s+(?<winningNumbers>[\d\s]+)\|\s+(?<cardNumbers>[\d\s]+)", RegexOptions.Multiline)
let numberRegex = Regex(@"(?<number>\d+)")

type Card =
    { Id: int
      WinningNumbers : int Set
      CardNumbers : int Set }

let parseCard line : Card =
    let parseNumbers =
        fun (nsStr : string) ->
            nsStr
            |> numberRegex.Matches
            |> Seq.map (fun m -> m |> Match.getValue "number" |> int)
            |> Set.ofSeq
    
    let m = cardRegex.Match(line)
    let id = m |> Match.getValue "id" |> int
    let winningNumbers = m |> Match.getValue "winningNumbers" |> parseNumbers
    let cardNumbers = m |> Match.getValue "cardNumbers" |> parseNumbers

    { Id = id
      WinningNumbers = winningNumbers
      CardNumbers = cardNumbers }

let getCardMatchingNumbersCount (card : Card) =
    card.WinningNumbers
    |> Set.intersect card.CardNumbers
    |> Set.count

let getCardPoints (card : Card) =
    let matchingNumbersCount =
        card
        |> getCardMatchingNumbersCount
        |> float
    if (matchingNumbersCount > 0) then
        2. ** (matchingNumbersCount - 1.)
        |> Math.Round
        |> int
        |> Some
    else
        None

let calculateResult_01 ss =
    ss
    |> List.map parseCard
    |> List.choose getCardPoints
    |> List.sum

let calculateResult_02 ss =
    let cards =
        ss
        |> List.map parseCard
        |> List.sortBy _.Id
    
    let cardsCopiesById = cards.ToDictionary((_.Id), (fun _ -> 1))
    
    for c in cards do
        let matchingNumbersCount = getCardMatchingNumbersCount c 
        if matchingNumbersCount > 0 then
            let currentCardCopies = cardsCopiesById.[c.Id]
            for i in [ 1 .. matchingNumbersCount ] do
                cardsCopiesById.[c.Id + i] <- cardsCopiesById.[c.Id + i] + currentCardCopies
    
    cardsCopiesById.Values
    |> Seq.sum

// wrappers
let solve_puzzle_1 () =
    
    File.readLines @"./inputs/day04.txt"
    |> Seq.toList
    |> calculateResult_01
    |> printfn "Day04, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day04.txt"
    |> Seq.toList
    |> calculateResult_02
    |> printfn "Day04, puzzle 2 -> result = %A"