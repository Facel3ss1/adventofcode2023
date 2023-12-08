module Util

open System

let tryParseUInt64 (number: string) =
    match UInt64.TryParse(number) with
    | (true, number) -> Some number
    | _ -> None

let splitBy (separator: 'a) (list: 'a list) =
    let rec loop (groupSoFar: 'a list) (list: 'a list) : 'a list list =
        match list with
        | [] ->
            if List.isEmpty groupSoFar then
                []
            else
                [ List.rev groupSoFar ]
        | head :: tail when head = separator ->
            if List.isEmpty groupSoFar then
                loop [] tail
            else
                List.rev groupSoFar :: loop [] tail
        | head :: tail -> loop (head :: groupSoFar) tail

    loop [] list
