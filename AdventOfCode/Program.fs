﻿open System.IO

let part1Solver day =
    match day with
    | 1 -> Day1.solvePart1
    | 2 -> Day2.solvePart1
    | 3 -> Day3.solvePart1
    | 4 -> Day4.solvePart1
    | 5 -> Day5.solvePart1
    | 6 -> Day6.solvePart1
    | 7 -> Day7.solvePart1
    | 8 -> Day8.solvePart1
    | 9 -> Day9.solvePart1
    | 11 -> Day11.solvePart1
    | _ -> fun _ -> "not solved"

let part2Solver day =
    match day with
    | 1 -> Day1.solvePart2
    | 2 -> Day2.solvePart2
    | 3 -> Day3.solvePart2
    | 6 -> Day6.solvePart2
    | 7 -> Day7.solvePart2
    | 9 -> Day9.solvePart2
    | 11 -> Day11.solvePart2
    | _ -> fun _ -> "not solved"

let readLines (day: int) =
    let filename day =
        Path.Combine(__SOURCE_DIRECTORY__, $"Input/day{day}.txt")

    File.ReadAllLines(filename day) |> Array.toList

let printSolutions (day: int) =
    let lines = readLines day
    let part1Solution = part1Solver day <| lines
    printfn $"Part 1: {part1Solution}"
    let part2Solution = part2Solver day <| lines
    printfn $"Part 2: {part2Solution}"

[<EntryPoint>]
let main (args: string array) =
    match args with
    | [| day |] ->
        printSolutions (int day)
        0
    | _ ->
        eprintfn "usage: <day>"
        1
