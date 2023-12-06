module Util

open System

let tryParseUInt64 (number: string) =
    match UInt64.TryParse(number) with
    | (true, number) -> Some number
    | _ -> None
