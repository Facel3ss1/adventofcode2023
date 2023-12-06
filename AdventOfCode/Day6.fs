module Day6

type Race =
    { Time: uint64; RecordDistance: uint64 }

let parsePart1Races (lines: string list) : Race list =
    let (timesLine, distancesLine) =
        match lines with
        | [ timesLine; distancesLines ] -> (timesLine, distancesLines)
        | _ -> failwith "invalid race"

    let times = timesLine.Split(' ') |> Array.tail |> Array.choose Util.tryParseUInt64

    let distances =
        distancesLine.Split(' ') |> Array.tail |> Array.choose Util.tryParseUInt64

    Array.zip times distances
    |> Array.map (fun (time, distance) ->
        { Time = time
          RecordDistance = distance })
    |> Array.toList

let parsePart2Race (lines: string list) : Race =
    let (timesLine, distancesLine) =
        match lines with
        | [ timesLine; distancesLines ] -> (timesLine, distancesLines)
        | _ -> failwith "invalid race"

    let time = timesLine.Split(' ') |> Array.tail |> Array.reduce (+) |> uint64
    let distance = distancesLine.Split(' ') |> Array.tail |> Array.reduce (+) |> uint64

    { Time = time
      RecordDistance = distance }

let waysToWin (race: Race) : int =
    let holdTimes = [ 1UL .. race.Time - 1UL ]
    let distance (holdTime: uint64) = holdTime * (race.Time - holdTime)
    let isWinningGame (d: uint64) : bool = d > race.RecordDistance

    let distances = holdTimes |> List.map distance
    let startIndex = distances |> List.findIndex isWinningGame
    let endIndex = distances |> List.findIndexBack isWinningGame

    (endIndex - startIndex) + 1

let solvePart1 (lines: string list) : string =
    parsePart1Races lines |> List.map waysToWin |> List.reduce (*) |> string

let solvePart2 (lines: string list) : string =
    parsePart2Race lines |> waysToWin |> string
