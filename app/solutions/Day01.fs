module App.Solutions.Day01

open App.Helpers

let fixSpelledDigits (str : string) =
    let spelledToDigits =
        [ "one", '1'
          "two", '2'
          "three", '3'
          "four", '4'
          "five", '5'
          "six", '6'
          "seven", '7'
          "eight", '8'
          "nine", '9' ]
    
    let rec loop acc i =
        if i >= str.Length then
            acc |> List.rev
        else
            let remainingStr = str.Substring i
            let matchingTuple' =
                spelledToDigits
                |> List.tryFind (fun (text, _) -> remainingStr.StartsWith text)
            let newAcc =
                match matchingTuple' with
                | None -> str[i]::acc
                | Some (_, digit) -> digit::acc
            
            loop newAcc (i + 1)
    
    loop [] 0

let searchCalibrationValues ss =
    ss
    |> List.map (List.filter (
        function
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
            | _ -> false))
    |> List.choose (fun s ->
        if s.Length >= 1 then
            $"{s[0]}{s[s.Length - 1]}" |> Some
        else
            None)
    |> List.map int
    |> List.sum

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day01.txt"
    |> Seq.toList
    |> List.map Seq.toList
    |> searchCalibrationValues
    |> printfn "Day01, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day01.txt"
    |> Seq.toList
    |> List.map fixSpelledDigits
    |> searchCalibrationValues
    |> printfn "Day01, puzzle 2 -> result = %A"