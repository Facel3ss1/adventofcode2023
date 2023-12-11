module Day5

type Range =
    { StartInclusive: uint32
      EndInclusive: uint32 }

type Mapping =
    { DestinationRangeStart: uint32
      SourceRangeStart: uint32
      RangeLength: uint32 }

type Almanac =
    { Seeds: uint32 list
      Maps: Mapping array list }

let parseAlmanac (lines: string list) : Almanac =
    let groups = Util.splitBy "" lines
    let seedsLine = List.head groups |> List.head
    let mapsLines = List.tail groups

    let seeds = seedsLine.Split(' ') |> Array.tail |> Array.map uint32 |> Array.toList

    let parseMapping (mappingString: string) =
        match mappingString.Split(' ') |> Array.map uint32 with
        | [| destinationRangeStart; sourceRangeStart; rangeLength |] ->
            { DestinationRangeStart = destinationRangeStart
              SourceRangeStart = sourceRangeStart
              RangeLength = rangeLength }
        | _ -> failwith "invalid mapping"

    let maps =
        mapsLines
        |> List.map (List.tail >> List.map parseMapping)
        |> List.map (List.sortBy (_.SourceRangeStart))
        |> List.map List.toArray

    { Seeds = seeds; Maps = maps }

let applyMap (seed: uint32) (map: Mapping array) : uint32 =
    let sourceRange (mapping: Mapping) =
        { StartInclusive = mapping.SourceRangeStart
          EndInclusive = mapping.SourceRangeStart + mapping.RangeLength - 1u }

    let sourceRanges: Range array = map |> Array.map sourceRange

    let rec binarySearch lowerBound upperBound =
        if lowerBound > upperBound then
            None
        else
            let midPoint = (upperBound + lowerBound) / 2
            let currentRange = sourceRanges[midPoint]

            if seed > currentRange.EndInclusive then
                binarySearch (midPoint + 1) upperBound
            else if seed < currentRange.StartInclusive then
                binarySearch lowerBound (midPoint - 1)
            else
                Some midPoint

    let index = binarySearch 0 (Array.length sourceRanges - 1)

    let applyMapping (mapping: Mapping) (seed: uint32) =
        seed + (mapping.DestinationRangeStart - mapping.SourceRangeStart)

    match index with
    | Some index -> applyMapping map[index] seed
    | None -> seed

let mapSeed (maps: Mapping array list) (seed: uint32) : uint32 = maps |> List.fold applyMap seed

let solvePart1 (lines: string list) : string =
    let almanac = parseAlmanac lines
    almanac.Seeds |> List.map (mapSeed almanac.Maps) |> List.min |> string

let solvePart2 (lines: string list) : string = "todo"
