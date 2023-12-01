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
