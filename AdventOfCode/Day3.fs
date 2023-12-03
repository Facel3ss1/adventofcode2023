module Day3

open System

type Coord = { Row: int; Column: int }

type PartNumber =
    { Number: int
      Row: int
      StartColumn: int
      EndColumn: int }

let isDigit c = Char.IsDigit(c)

let extractSymbols (lines: string list) : Map<Coord, char> =
    let extractSymbolsFromRow (row: int, line: string) : (Coord * char) list =
        let isSymbol (c: char) = c <> '.' && not (isDigit c)

        let toEntry (column: int, symbol: char) =
            ({ Row = row; Column = column }, symbol)

        line
        |> Seq.indexed
        |> Seq.filter (snd >> isSymbol)
        |> Seq.map toEntry
        |> Seq.toList

    lines |> List.indexed |> List.collect extractSymbolsFromRow |> Map

let extractPartNumbers (lines: string list) : PartNumber list =
    let extractPartNumbersFromRow (row: int, line: string) : PartNumber list =
        let digits = line |> Seq.indexed |> Seq.filter (snd >> isDigit)

        let folder (numbers: (string * int * int) list) (currentColumn: int, digitChar: char) =
            let digit = string digitChar
            let newNumber = (digit, currentColumn, currentColumn)

            match numbers with
            | (digits, startColumn, endColumn) as currentNumber :: rest ->
                if currentColumn = endColumn + 1 then
                    (digits + digit, startColumn, currentColumn) :: rest
                else
                    newNumber :: currentNumber :: rest
            | [] -> [ newNumber ]

        let numbers: (string * int * int) list = digits |> Seq.fold folder []

        let toPartNumber (digits: string, startColumn: int, endColumn: int) =
            { Number = Int32.Parse(digits)
              Row = row
              StartColumn = startColumn
              EndColumn = endColumn }

        numbers |> Seq.map toPartNumber |> Seq.toList |> List.rev

    lines |> List.indexed |> List.collect extractPartNumbersFromRow

let getAdjacentCoords (partNumber: PartNumber) : Coord list =
    let rows = [ partNumber.Row - 1 .. partNumber.Row + 1 ]
    let columns = [ partNumber.StartColumn - 1 .. partNumber.EndColumn + 1 ]

    let coordsForRow (row: int) =
        columns |> List.map (fun column -> { Row = row; Column = column })

    let isOnNumber { Row = row; Column = column } =
        row = partNumber.Row
        && column >= partNumber.StartColumn
        && column <= partNumber.EndColumn

    rows |> List.collect coordsForRow |> List.filter (isOnNumber >> not)

let getAdjacentSymbols (symbols: Map<Coord, char>) (partNumber: PartNumber) : (Coord * char) list =
    let symbolEntry (coord: Coord) =
        symbols |> Map.tryFind coord |> Option.map (fun symbol -> (coord, symbol))

    getAdjacentCoords partNumber |> List.choose symbolEntry

let solvePart1 (lines: string list) : string =
    let symbols = extractSymbols lines
    let partNumbers = extractPartNumbers lines

    partNumbers
    |> List.filter (getAdjacentSymbols symbols >> List.isEmpty >> not)
    |> List.map (fun partNumber -> partNumber.Number)
    |> List.sum
    |> string

let solvePart2 (lines: string list) : string =
    let symbols = extractSymbols lines
    let partNumbers = extractPartNumbers lines

    let adjacentEntries (partNumber: PartNumber) : (char * Coord * PartNumber) list =
        getAdjacentSymbols symbols partNumber
        |> List.map (fun (coord, symbol) -> (symbol, coord, partNumber))

    let gearEntries: (Coord * PartNumber) list =
        partNumbers
        |> List.collect adjacentEntries
        |> List.filter (fun (symbol, _, _) -> symbol = '*')
        |> List.map (fun (_, coord, partNumber) -> (coord, partNumber))

    let gearPartNumbers: PartNumber list list =
        gearEntries
        |> List.groupBy fst
        |> List.map (snd >> List.map snd)
        |> List.filter (fun partNumbers -> List.length partNumbers = 2)

    gearPartNumbers
    |> List.map (List.map (fun partNumber -> partNumber.Number))
    |> List.map (List.reduce (*))
    |> List.sum
    |> string
