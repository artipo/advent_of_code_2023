module App.Solutions.Day01

open App.Helpers

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day01.txt"
    |> id
    |> printfn "Day01, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day01.txt"
    |> id
    |> printfn "Day01, puzzle 2 -> result = %A"