module Day11

type Coord = { Row: int; Column: int }

type Image =
    { Galaxies: Coord list
      EmptyRows: int list
      EmptyColumns: int list }

let extractGalaxies (lines: string list) : Coord list =
    let extractGalaxiesFromRow (row: int, line: string) : Coord list =
        let isGalaxy (c: char) = c = '#'
        let toCoord (column: int, _) = { Row = row; Column = column }

        line
        |> Seq.indexed
        |> Seq.filter (snd >> isGalaxy)
        |> Seq.map toCoord
        |> Seq.toList

    lines |> List.indexed |> List.collect extractGalaxiesFromRow

let extractEmptyRowsAndColumns (lines: string list) : int list * int list =
    let getEmptyRows (rows: char list list) : int list =
        let isRowEmpty (row: char list) = row |> List.forall (fun c -> c = '.')
        rows |> List.indexed |> List.filter (snd >> isRowEmpty) |> List.map fst

    let rows = lines |> List.map Seq.toList
    let emptyRows = rows |> getEmptyRows
    let emptyColumns = rows |> List.transpose |> getEmptyRows

    (emptyRows, emptyColumns)

let extractImage (lines: string list) : Image =
    let galaxies = extractGalaxies lines
    let (emptyRows, emptyColumns) = extractEmptyRowsAndColumns lines

    { Galaxies = galaxies
      EmptyRows = emptyRows
      EmptyColumns = emptyColumns }

let distance (emptyRows: int list) (emptyColumns: int list) (firstGalaxy: Coord, secondGalaxy: Coord) : int =
    let (leftGalaxy, rightGalaxy) =
        if firstGalaxy.Column < secondGalaxy.Column then
            (firstGalaxy, secondGalaxy)
        else
            (secondGalaxy, firstGalaxy)

    let (topGalaxy, bottomGalaxy) =
        if firstGalaxy.Row < secondGalaxy.Row then
            (firstGalaxy, secondGalaxy)
        else
            (secondGalaxy, firstGalaxy)

    let horizontalDistance = rightGalaxy.Column - leftGalaxy.Column
    let verticalDistance = bottomGalaxy.Row - topGalaxy.Row

    let overlappingRows =
        emptyRows
        |> List.filter (fun row -> topGalaxy.Row < row && row < bottomGalaxy.Row)

    let overlappingColumns =
        emptyColumns
        |> List.filter (fun column -> leftGalaxy.Column < column && column < rightGalaxy.Column)

    horizontalDistance
    + verticalDistance
    + List.length overlappingRows
    + List.length overlappingColumns

let solvePart1 (lines: string list) : string =
    let image = extractImage lines

    List.allPairs image.Galaxies image.Galaxies
    |> List.map (distance image.EmptyRows image.EmptyColumns)
    |> List.sum
    |> (fun sum -> sum / 2)
    |> string

let solvePart2 (lines: string list) : string = "todo"
