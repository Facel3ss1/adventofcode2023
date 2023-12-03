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

let isPartNumberAdjacentToSymbol (symbols: Map<Coord, char>) (partNumber: PartNumber) : bool =
    getAdjacentCoords partNumber
    |> List.exists (fun coord -> symbols |> Map.containsKey coord)

let solvePart1 (lines: string list) : string =
    let symbols = extractSymbols lines
    let partNumbers = extractPartNumbers lines

    partNumbers
    |> List.filter (isPartNumberAdjacentToSymbol symbols)
    |> List.map (fun partNumber -> partNumber.Number)
    |> List.sum
    |> string
