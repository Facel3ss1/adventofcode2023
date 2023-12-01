module Day1

open System

let removeLetters (line: string): int list =
    line |> Seq.toList |> List.filter Char.IsNumber |> List.map (string >> int)

// This is a 'parameterised active pattern' - let's you define your own pattern matching 🤯
let (|Prefix|_|) (prefix: string) (s : string) =
    if s.StartsWith(prefix) then
        Some(s.Substring(prefix.Length))
    else
        None

let rec consumeNumberWords (line: string): int list =
    let rest = line[1..]
    match line with
    | Prefix "one" _ -> 1 :: consumeNumberWords rest
    | Prefix "two" _ -> 2 :: consumeNumberWords rest
    | Prefix "three" _ -> 3 :: consumeNumberWords rest
    | Prefix "four" _ -> 4 :: consumeNumberWords rest
    | Prefix "five" _ -> 5 :: consumeNumberWords rest
    | Prefix "six" _ -> 6 :: consumeNumberWords rest
    | Prefix "seven" _ -> 7 :: consumeNumberWords rest
    | Prefix "eight" _ -> 8 :: consumeNumberWords rest
    | Prefix "nine" _ -> 9 :: consumeNumberWords rest
    | Prefix "1" _ -> 1 :: consumeNumberWords rest
    | Prefix "2" _ -> 2 :: consumeNumberWords rest
    | Prefix "3" _ -> 3 :: consumeNumberWords rest
    | Prefix "4" _ -> 4 :: consumeNumberWords rest
    | Prefix "5" _ -> 5 :: consumeNumberWords rest
    | Prefix "6" _ -> 6 :: consumeNumberWords rest
    | Prefix "7" _ -> 7 :: consumeNumberWords rest
    | Prefix "8" _ -> 8 :: consumeNumberWords rest
    | Prefix "9" _ -> 9 :: consumeNumberWords rest
    | "" -> []
    | _ -> consumeNumberWords rest

let firstAndLastDigit (digits: int list) =
    if List.isEmpty digits then
        0
    else
        let first = List.head digits
        let last = List.last digits
        10 * first + last

let solvePart1 (lines: string list) =
    lines
    |> List.map removeLetters
    |> List.map firstAndLastDigit
    |> List.sum
    |> string

let solvePart2 (lines: string list) =
    lines
    |> List.map consumeNumberWords
    |> List.map firstAndLastDigit
    |> List.sum
    |> string
