module Day1

open System

let inline charToInt (c: char) = int c - int '0'

let removeLetters (line: string): int list =
    line |> Seq.toList |> List.filter Char.IsNumber |> List.map charToInt

// This is a 'active pattern' - let's you define your own pattern matching 🤯
let (|StringPrefix|_|) (prefix: string) (s : string) =
    if s.StartsWith(prefix) then
        Some(s.Substring(prefix.Length))
    else
        None

let (|DigitPrefix|_|) (s : string) =
    if String.length s = 0 then
        None
    else if Char.IsNumber s[0] then
        Some(charToInt s[0])
    else
        None

let rec consumeNumberWords (line: string): int list =
    let rest = line[1..]
    match line with
    | StringPrefix "one" _ -> 1 :: consumeNumberWords rest
    | StringPrefix "two" _ -> 2 :: consumeNumberWords rest
    | StringPrefix "three" _ -> 3 :: consumeNumberWords rest
    | StringPrefix "four" _ -> 4 :: consumeNumberWords rest
    | StringPrefix "five" _ -> 5 :: consumeNumberWords rest
    | StringPrefix "six" _ -> 6 :: consumeNumberWords rest
    | StringPrefix "seven" _ -> 7 :: consumeNumberWords rest
    | StringPrefix "eight" _ -> 8 :: consumeNumberWords rest
    | StringPrefix "nine" _ -> 9 :: consumeNumberWords rest
    | DigitPrefix digit -> digit :: consumeNumberWords rest
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
