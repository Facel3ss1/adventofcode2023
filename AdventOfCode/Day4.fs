module Day4

open System

type Card =
    { Id: int
      WinningNumbers: Set<int>
      MyNumbers: Set<int> }

let parseCard (line: string) : Card =
    let (cardPrefix, numbers) =
        match line.Split(": ") with
        | [| cardPrefix; numbers |] -> (cardPrefix, numbers)
        | _ -> failwith "invalid line"

    let id = Int32.Parse(cardPrefix[4..])

    let (winningNumbers, myNumbers) =
        match numbers.Split(" | ") with
        | [| winningNumbers; myNumbers |] -> (winningNumbers, myNumbers)
        | _ -> failwith "invalid numbers"

    let parseNumber (number: string) =
        match Int32.TryParse(number) with
        | (true, number) -> Some number
        | _ -> None

    let parseNumbers (numbers: string) : Set<int> =
        numbers.Split(' ') |> Array.choose parseNumber |> Set.ofArray

    { Id = id
      WinningNumbers = parseNumbers winningNumbers
      MyNumbers = parseNumbers myNumbers }

let numberOfWinningNumbers (card: Card) : int =
    card.WinningNumbers |> Set.intersect card.MyNumbers |> Set.count

let cardPoints (card: Card) : int =
    pown 2 (numberOfWinningNumbers card - 1)

let solvePart1 (lines: string list) : string =
    lines |> List.map (parseCard >> cardPoints) |> List.sum |> string
