module Day9

let parseSequences (lines: string list) : int list list =
    let parseLine (line: string) =
        line.Split(' ') |> Array.map int |> Array.toList

    lines |> List.map parseLine

let differences (sequence: int list) : int list =
    sequence |> List.pairwise |> List.map (fun (a, b) -> b - a)

let getEdgeSteps (getEdgeFn: int list -> int) (sequence: int list) : int list =
    let rec recurse (currentLevel: int list) =
        if List.forall (fun a -> a = 0) currentLevel then
            [ 0 ]
        else
            let edgeStep = getEdgeFn currentLevel
            edgeStep :: recurse (differences currentLevel)

    recurse sequence |> List.rev

let extrapolate (getEdgeFn: int list -> int) (combineFn: int -> int -> int) (sequence: int list) : int =
    let edgeSteps = getEdgeSteps getEdgeFn sequence
    List.reduce combineFn edgeSteps

let solve (getEdgeFn: int list -> int) (combineFn: int -> int -> int) (sequences: int list list) : int =
    sequences |> List.map (extrapolate getEdgeFn combineFn) |> List.sum

let solvePart1 (lines: string list) : string =
    let sequences = parseSequences lines
    solve List.last (+) sequences |> string

let solvePart2 (lines: string list) : string =
    let sequences = parseSequences lines
    solve List.head (fun a b -> b - a) sequences |> string
