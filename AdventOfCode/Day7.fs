module Day7

type Card =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | JackOrJoker
    | Queen
    | King
    | Ace

type Hand = Card * Card * Card * Card * Card

type HandAndBid = { Hand: Hand; Bid: int }

type HandType =
    | HighCard = 1
    | OnePair = 2
    | TwoPair = 3
    | ThreeOfAKind = 4
    | FullHouse = 5
    | FourOfAKind = 6
    | FiveOfAKind = 7

let cardRankPart1 (card: Card) : int =
    match card with
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | JackOrJoker -> 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14

let cardRankPart2 (card: Card) : int =
    match card with
    | JackOrJoker -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Queen -> 12
    | King -> 13
    | Ace -> 14

let parseCard (c: char) : Card =
    match c with
    | '2' -> Card.Two
    | '3' -> Card.Three
    | '4' -> Card.Four
    | '5' -> Card.Five
    | '6' -> Card.Six
    | '7' -> Card.Seven
    | '8' -> Card.Eight
    | '9' -> Card.Nine
    | 'T' -> Card.Ten
    | 'J' -> Card.JackOrJoker
    | 'Q' -> Card.Queen
    | 'K' -> Card.King
    | 'A' -> Card.Ace
    | _ -> failwith "invalid card"

let parseHand (s: string) : Hand =
    let hand = s |> Seq.map parseCard |> Seq.toArray

    match hand with
    | [| first; second; third; fourth; fifth |] -> (first, second, third, fourth, fifth)
    | _ -> failwith "invalid hand"

let parseHandAndBid (line: string) : HandAndBid =
    let (hand, bid) =
        match line.Split(' ') with
        | [| hand; bid |] -> (hand, bid)
        | _ -> failwith "invalid line"

    { Hand = parseHand hand; Bid = int bid }

let handToList (hand: Hand) : Card list =
    let (first, second, third, fourth, fifth) = hand
    [ first; second; third; fourth; fifth ]

let getGroupCountsPart1 (hand: Hand) : int list =
    handToList hand
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.sortDescending

let getGroupCountsPart2 =
    let getGroupCountsWithJokers (hand: Hand) : int * int list =
        let groupCount = snd >> List.length

        let cardGroups = handToList hand |> List.groupBy id

        let (jokerGroup, otherGroups) =
            cardGroups |> List.partition (fun (card, _) -> card = Card.JackOrJoker)

        let numberOfJokers =
            List.tryHead jokerGroup |> Option.map groupCount |> Option.defaultValue 0

        let groupCounts = otherGroups |> List.map groupCount |> List.sortDescending

        (numberOfJokers, groupCounts)

    let greedyGroupCounts (numberOfJokers: int, groupCounts: int list) : int list =
        match groupCounts with
        | largestGroup :: otherGroups -> numberOfJokers + largestGroup :: otherGroups
        | [] -> [ numberOfJokers ]

    getGroupCountsWithJokers >> greedyGroupCounts

let getHandType (groupCounts: int list) : HandType =
    match groupCounts with
    | [ 5 ] -> HandType.FiveOfAKind
    | [ 4; 1 ] -> HandType.FourOfAKind
    | [ 3; 2 ] -> HandType.FullHouse
    | [ 3; 1; 1 ] -> HandType.ThreeOfAKind
    | [ 2; 2; 1 ] -> HandType.TwoPair
    | [ 2; 1; 1; 1 ] -> HandType.OnePair
    | [ 1; 1; 1; 1; 1 ] -> HandType.HighCard
    | _ -> failwith "unreachable"

let comparisonList cardRankFn getGroupCountsFn hand =
    let handType = hand |> getGroupCountsFn |> getHandType |> int
    let ranks = hand |> handToList |> List.map cardRankFn
    handType :: ranks

let compareHandsAndBids comparisonListFn leftHandAndBid rightHandAndBid =
    let leftComparisonList = comparisonListFn leftHandAndBid.Hand
    let rightComparisonList = comparisonListFn rightHandAndBid.Hand
    List.compareWith compare leftComparisonList rightComparisonList

let solve cardRankFn getGroupCountsFn handsAndBids =
    let comparisonListFn = comparisonList cardRankFn getGroupCountsFn
    let compareHandsAndBidsFn = compareHandsAndBids comparisonListFn

    handsAndBids
    |> List.sortWith compareHandsAndBidsFn
    |> List.mapi (fun i handAndBid -> (i + 1) * handAndBid.Bid)
    |> List.sum

let solvePart1 (lines: string list) : string =
    let handsAndBids = lines |> List.map parseHandAndBid
    solve cardRankPart1 getGroupCountsPart1 handsAndBids |> string

let solvePart2 (lines: string list) : string =
    let handsAndBids = lines |> List.map parseHandAndBid
    solve cardRankPart2 getGroupCountsPart2 handsAndBids |> string
