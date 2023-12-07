#load "Helpers.fs"

//fsi.ShowProperties <- false
//fsi.ShowIEnumerable <- false
//fsi.ShowDeclarationValues <- false

open System
open Helpers


let lines = Helpers.readFile "7"

lines |> List.iter Console.WriteLine

let faceCardMap =
    [ ('T', 10)
      //('J', 11)
      ('J', 1)
      ('Q', 12)
      ('K', 13)
      ('A', 14) ]
    |> Map.ofList

type Hand = { Cards: string; BidAmount: int }

let parseHand line =
    let split = line |> String.split " "

    match split with
    | [| cards; bid |] -> { Cards = cards; BidAmount = int bid }
    | other -> failwith $"Expected two elements! %A{other}"

let getCardValue card =
    faceCardMap
    |> Map.tryFind card
    |> Option.defaultWith (fun _ -> int $"{card}")

let getStrength hand =
    let jokers, mainHand = hand |> List.ofSeq |> List.partition ((=) 'J')
    let jokersLength = jokers.Length

    let groupNumbers =
        mainHand
        |> Seq.groupBy id
        |> Seq.map (snd >> Seq.length)
        |> Seq.sortDescending
        |> Seq.toList

    let groupNumbers =
        match groupNumbers with
        | head :: rest -> head + jokersLength :: rest
        | [] -> [ jokersLength ]


    let strength =
        match groupNumbers with
        | 5 :: _ -> 7
        | 4 :: _ -> 6
        | 3 :: 2 :: _ -> 5
        | 3 :: _ -> 4
        | 2 :: 2 :: _ -> 3
        | 2 :: _ -> 2
        | 1 :: _ -> 1
        | other -> failwith $"Invalid number of elements! %A{other}"

    strength

let getOrder (hand: Hand) =
    let cards = hand.Cards
    let strength = getStrength cards
    let cardValues = cards |> Seq.map getCardValue |> Seq.toList

    strength, cardValues

let allHands = lines |> List.map parseHand

let allHandsRanked =
    allHands
    |> List.sortBy getOrder
    |> List.indexed
    |> List.map (fun (i, hand) -> (i + 1) * hand.BidAmount)
    |> List.sum

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{allHandsRanked}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
