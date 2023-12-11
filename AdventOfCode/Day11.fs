module Day11

type Coord = { Row: uint64; Column: uint64 }

type Image =
    { Galaxies: Coord list
      EmptyRows: uint64 list
      EmptyColumns: uint64 list }

let extractGalaxies (lines: string list) : Coord list =
    let extractGalaxiesFromRow (row: int, line: string) : Coord list =
        let isGalaxy (c: char) = c = '#'

        let toCoord (column: int, _) =
            { Row = uint64 row
              Column = uint64 column }

        line
        |> Seq.indexed
        |> Seq.filter (snd >> isGalaxy)
        |> Seq.map toCoord
        |> Seq.toList

    lines |> List.indexed |> List.collect extractGalaxiesFromRow

let extractEmptyRowsAndColumns (lines: string list) : uint64 list * uint64 list =
    let getEmptyRows (rows: char list list) : uint64 list =
        let isRowEmpty (row: char list) = row |> List.forall (fun c -> c = '.')

        rows
        |> List.indexed
        |> List.filter (snd >> isRowEmpty)
        |> List.map (fst >> uint64)

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

let distance
    (expansionMultiplier: uint64)
    (emptyRows: uint64 list)
    (emptyColumns: uint64 list)
    (firstGalaxy: Coord, secondGalaxy: Coord)
    : uint64 =
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
        |> List.length
        |> uint64

    let overlappingColumns =
        emptyColumns
        |> List.filter (fun column -> leftGalaxy.Column < column && column < rightGalaxy.Column)
        |> List.length
        |> uint64

    horizontalDistance
    + verticalDistance
    + (overlappingRows + overlappingColumns) * (expansionMultiplier - 1UL)

let solve (expansionMultiplier: uint64) (image: Image) : uint64 =
    List.allPairs image.Galaxies image.Galaxies
    |> List.map (distance expansionMultiplier image.EmptyRows image.EmptyColumns)
    |> List.sum
    |> (fun sum -> sum / 2UL)

let solvePart1 (lines: string list) : string =
    let image = extractImage lines
    image |> solve 2UL |> string

let solvePart2 (lines: string list) : string =
    let image = extractImage lines
    image |> solve 1000000UL |> string
