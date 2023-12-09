module Day9

let parseSequences (lines: string list) : int list list =
    let parseLine (line: string) =
        line.Split(' ') |> Array.map int |> Array.toList

    lines |> List.map parseLine

let differences (sequence: int list) : int list =
    sequence |> List.pairwise |> List.map (fun (a, b) -> b - a)

let getLastSteps (sequence: int list) : int list =
    let rec recurse (currentLevel: int list) =
        if List.forall (fun a -> a = 0) currentLevel then
            [ 0 ]
        else
            let lastStep = List.last currentLevel
            lastStep :: recurse (differences currentLevel)

    recurse sequence |> List.rev

let extrapolate (sequence: int list) : int =
    let lastSteps = getLastSteps sequence
    List.sum lastSteps

let solvePart1 (lines: string list) : string =
    let sequences = parseSequences lines
    sequences |> List.map extrapolate |> List.sum |> string
