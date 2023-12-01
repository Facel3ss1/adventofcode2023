#load "Day1.fs"

open System.IO

let solvePart1 (day: int) (lines: string list) =
    match day with
    | 1 -> Day1.solvePart1 lines
    | _ -> "invalid day"

let solvePart2 (day: int) (lines: string list) =
    match day with
    | 1 -> Day1.solvePart2 lines
    | _ -> "invalid day"

let readLines (day: int) =
    let filename day =
        Path.Combine(__SOURCE_DIRECTORY__, $"Input/day{day}.txt")

    File.ReadAllLines(filename day) |> Array.toList

let printSolutions (day: int) =
    let lines = readLines day
    let part1Solution = solvePart1 day lines
    let part2Solution = solvePart2 day lines
    printfn $"Part 1: {part1Solution}"
    printfn $"Part 2: {part2Solution}"

match fsi.CommandLineArgs with
| [| _; day |] -> printSolutions (int day)
| _ -> eprintfn "usage: <day>"
