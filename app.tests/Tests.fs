module Tests

open FsUnit.Xunit
open Xunit

module Day01 =

    open App.Solutions.Day01

    [<Fact>]
    let ``day 01, puzzle 1`` () =
        [ "1abc2"
          "pqr3stu8vwx"
          "a1b2c3d4e5f"
          "treb7uchet" ]
        |> List.map Seq.toList
        |> searchCalibrationValues
        |> should equal 142

    [<Fact>]
    let ``day 01, puzzle 2`` () =
        [ "two1nine"
          "eightwothree"
          "abcone2threexyz"
          "xtwone3four"
          "4nineeightseven2"
          "zoneight234"
          "7pqrstsixteen" ]
        |> List.map fixSpelledDigits
        |> searchCalibrationValues
        |> should equal 282

module Day02 =

    open App.Solutions.Day02

    [<Fact>]
    let ``day 02, puzzle 1`` () =
        let diceAvailability =
            [ Red, 12u
              Green, 13u
              Blue, 14u ]
            |> Map.ofList
        
        [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
          "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
          "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
          "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
          "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]
        |> calculateResult_01 diceAvailability
        |> should equal 8u

    [<Fact>]
    let ``day 02, puzzle 2`` () =
        [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
          "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
          "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
          "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
          "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]
        |> calculateResult_02
        |> should equal 2286u

module Day03 =

    open App.Solutions.Day03

    [<Fact>]
    let ``day 03, puzzle 1`` () =
        
        [ "467..114.."
          "...*......"
          "..35..633."
          "......#..."
          "617*......"
          ".....+.58."
          "..592....."
          "......755."
          "...$.*...."
          ".664.598.." ]
        |> calculateResult_01
        |> should equal 4361

    [<Fact>]
    let ``day 03, puzzle 2`` () =
        
        [ "467..114.."
          "...*......"
          "..35..633."
          "......#..."
          "617*......"
          ".....+.58."
          "..592....."
          "......755."
          "...$.*...."
          ".664.598.." ]
        |> calculateResult_02
        |> should equal 467835
