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

module Day04 =

    open App.Solutions.Day04
    
    let input =
        [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
          "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
          "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
          "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
          "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
          "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" ]
    
    [<Fact>]
    let ``day 04, puzzle 1`` () =
        
        input
        |> calculateResult_01
        |> should equal 13
    
    [<Fact>]
    let ``day 04, puzzle 2`` () =
        
        input
        |> calculateResult_02
        |> should equal 30