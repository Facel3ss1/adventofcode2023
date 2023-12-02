module Day1

open System

let inline charToInt (c: char) = int c - int '0'

let extractDigits (line: string) : int list =
    line |> Seq.toList |> List.filter Char.IsNumber |> List.map charToInt

// This is a 'active pattern' - let's you define your own pattern matching 🤯
let (|StringPrefix|_|) (prefix: string) (s: string) =
    if s.StartsWith(prefix) then Some() else None

let (|DigitPrefix|_|) (s: string) =
    if String.length s = 0 then None
    else if Char.IsNumber s[0] then Some(charToInt s[0])
    else None

let rec extractWordsAndDigits (line: string) : int list =
    let rest = line[1..]

    match line with
    | StringPrefix "one" -> 1 :: extractWordsAndDigits rest
    | StringPrefix "two" -> 2 :: extractWordsAndDigits rest
    | StringPrefix "three" -> 3 :: extractWordsAndDigits rest
    | StringPrefix "four" -> 4 :: extractWordsAndDigits rest
    | StringPrefix "five" -> 5 :: extractWordsAndDigits rest
    | StringPrefix "six" -> 6 :: extractWordsAndDigits rest
    | StringPrefix "seven" -> 7 :: extractWordsAndDigits rest
    | StringPrefix "eight" -> 8 :: extractWordsAndDigits rest
    | StringPrefix "nine" -> 9 :: extractWordsAndDigits rest
    | DigitPrefix digit -> digit :: extractWordsAndDigits rest
    | "" -> []
    | _ -> extractWordsAndDigits rest

let firstAndLastDigit (digits: int list) =
    if List.isEmpty digits then
        0
    else
        let first = List.head digits
        let last = List.last digits
        10 * first + last

let solve (parseLine: string -> int list) (lines: string list) =
    lines |> List.map parseLine |> List.map firstAndLastDigit |> List.sum |> string

let solvePart1 = solve extractDigits

let solvePart2 = solve extractWordsAndDigits
