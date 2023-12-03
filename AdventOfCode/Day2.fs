module Day2

open System

type CubeSet = { Red: int; Green: int; Blue: int }

type Game = { Id: int; Sets: CubeSet list }

let parseGame (line: string) =
    let (gamePrefix, cubeSets) =
        match line.Split(": ") with
        | [| game; cubeSets |] -> (game, cubeSets)
        | _ -> failwith "invalid line"

    let id = Int32.Parse(gamePrefix[4..])

    let parseCube (cube: string) =
        match cube.Split(' ') with
        | [| amount; color |] -> (color, Int32.Parse(amount))
        | _ -> failwith "invalid cube"

    let parseCubeSet (input: string) =
        let cubes = input.Split(", ") |> Array.map parseCube

        let folder (red, green, blue) (color, amount) =
            match color with
            | "red" -> (amount, green, blue)
            | "green" -> (red, amount, blue)
            | "blue" -> (red, green, amount)
            | _ -> failwith "invalid color"

        let (red, green, blue) = cubes |> Array.fold folder (0, 0, 0)

        { Red = red
          Green = green
          Blue = blue }

    let sets = cubeSets.Split("; ") |> Array.map parseCubeSet |> Seq.toList

    { Id = id; Sets = sets }

let isSetPossible (bag: CubeSet) (set: CubeSet) =
    set.Red <= bag.Red && set.Green <= bag.Green && set.Blue <= bag.Blue

let isGamePossible (bag: CubeSet) (game: Game) =
    game.Sets |> List.map (isSetPossible bag) |> List.reduce (&&)

let power (set: CubeSet) = set.Red * set.Green * set.Blue

let minimumCubes (game: Game) =
    let reducer (leftSet: CubeSet) (rightSet: CubeSet) =
        { Red = max leftSet.Red rightSet.Red
          Green = max leftSet.Green rightSet.Green
          Blue = max leftSet.Blue rightSet.Blue }

    game.Sets |> List.reduce reducer

let solvePart1 (lines: string list) : string =
    let games = lines |> List.map parseGame

    games
    |> List.filter (isGamePossible { Red = 12; Green = 13; Blue = 14 })
    |> List.map (fun game -> game.Id)
    |> List.sum
    |> string

let solvePart2 (lines: string list) : string =
    let games = lines |> List.map parseGame

    games |> List.map (minimumCubes >> power) |> List.sum |> string
