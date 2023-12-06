module Util

open System

let tryParseInt (number: string) =
    match Int32.TryParse(number) with
    | (true, number) -> Some number
    | _ -> None
