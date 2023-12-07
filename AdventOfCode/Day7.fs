module Day7

type Card =
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    | Seven = 7
    | Eight = 8
    | Nine = 9
    | Ten = 10
    | Jack = 11
    | Queen = 12
    | King = 13
    | Ace = 14

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
    | 'J' -> Card.Jack
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

let groupCounts (hand: Hand) : int list =
    handToList hand
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.sortDescending

let getHandType (hand: Hand) : HandType =
    match groupCounts hand with
    | [ 5 ] -> HandType.FiveOfAKind
    | [ 4; 1 ] -> HandType.FourOfAKind
    | [ 3; 2 ] -> HandType.FullHouse
    | [ 3; 1; 1 ] -> HandType.ThreeOfAKind
    | [ 2; 2; 1 ] -> HandType.TwoPair
    | [ 2; 1; 1; 1 ] -> HandType.OnePair
    | [ 1; 1; 1; 1; 1 ] -> HandType.HighCard
    | _ -> failwith "unreachable"

let compareHandsAndBids (leftHandAndBid: HandAndBid) (rightHandAndBid: HandAndBid) : int =
    let leftHand = leftHandAndBid.Hand
    let rightHand = rightHandAndBid.Hand
    let leftHandType = getHandType leftHand |> int
    let rightHandType = getHandType rightHand |> int
    let leftRanks = handToList leftHand |> List.map int
    let rightRanks = handToList rightHand |> List.map int
    List.compareWith compare (leftHandType :: leftRanks) (rightHandType :: rightRanks)

let solvePart1 (lines: string list) : string =
    let handsAndBids = lines |> List.map parseHandAndBid

    handsAndBids
    |> List.sortWith compareHandsAndBids
    |> List.mapi (fun i handAndBid -> (i + 1) * handAndBid.Bid)
    |> List.sum
    |> string
