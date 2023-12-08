module Day8

type Instruction =
    | Left
    | Right

type Input =
    { Instructions: Instruction array
      Network: Map<string, string * string> }

let parseInstruction (c: char) : Instruction =
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "invalid instruction"

let parseNetworkLine (line: string) : string * (string * string) =
    let (node, neighbours) =
        match line.Split(" = ") with
        | [| node; neighbours |] -> (node, neighbours)
        | _ -> failwith "invalid network line"

    let (leftNeighbour, rightNeighbour) =
        match neighbours.Trim([| '('; ')' |]).Split(", ") with
        | [| leftNeighbour; rightNeighbour |] -> (leftNeighbour, rightNeighbour)
        | _ -> failwith "invalid neighbours"

    (node, (leftNeighbour, rightNeighbour))

let parseInput (lines: string list) : Input =
    let (instructionsLine, networkLines) =
        match Util.splitBy "" lines with
        | [ instructionsLine :: _; networkLines ] -> (instructionsLine, networkLines)
        | _ -> failwith "invalid input"

    let instructions = instructionsLine |> Seq.map parseInstruction |> Seq.toArray
    let network = networkLines |> List.map parseNetworkLine |> Map

    { Instructions = instructions
      Network = network }

let solvePart1 (lines: string list) : string =
    let input = parseInput lines

    let mutable instructionIndex = 0
    let mutable currentNode = "AAA"
    let mutable numberOfJumps = 0

    while currentNode <> "ZZZ" do
        let (leftNeighbour, rightNeighbour) = input.Network |> Map.find currentNode

        let currentInstruction = Array.get input.Instructions instructionIndex

        currentNode <-
            match currentInstruction with
            | Left -> leftNeighbour
            | Right -> rightNeighbour

        instructionIndex <- (instructionIndex + 1) % (Array.length input.Instructions)
        numberOfJumps <- numberOfJumps + 1

    numberOfJumps |> string
