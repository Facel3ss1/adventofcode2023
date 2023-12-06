module Day6

type Race = { Time: int; RecordDistance: int }

let parseRaces (lines: string list) : Race list =
    let (timesLine, distancesLine) =
        match lines with
        | [ timesLine; distancesLines ] -> (timesLine, distancesLines)
        | _ -> failwith "invalid race"

    let times = timesLine.Split(' ') |> Array.tail |> Array.choose Util.tryParseInt

    let distances =
        distancesLine.Split(' ') |> Array.tail |> Array.choose Util.tryParseInt

    Array.zip times distances
    |> Array.map (fun (time, distance) ->
        { Time = time
          RecordDistance = distance })
    |> Array.toList

let waysToWin (race: Race) : int =
    let holdTimes = [ 1 .. race.Time - 1 ]
    let distance (holdTime: int) = holdTime * (race.Time - holdTime)

    holdTimes
    |> List.map distance
    |> List.filter (fun d -> d > race.RecordDistance)
    |> List.length

let solvePart1 (lines: string list) : string =
    let races = parseRaces lines

    races |> List.map waysToWin |> List.reduce (*) |> string
