open System.Numerics


#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers

let fileName = $"4"

let lines = Helpers.readFile fileName

type ScratchCard =
    { Id: int
      WinningNumbers: int Set
      Numbers: int Set }

let getCountWinningNumbers (sc: ScratchCard) =
    let overlap = Set.intersect sc.WinningNumbers sc.Numbers
    let len = overlap |> Set.count
    len

let getWinningScore (sc: ScratchCard) =
    let len = getCountWinningNumbers sc
    let power = (len - 1)

    match len with
    | 0 -> 0
    | _ -> pown 2 power



let card = "Card "

let parseNumbers str =
    str |> String.split " " |> Seq.map int |> set

let parseLine (line: string) =
    let sansCard = line.Substring(card.Length)
    let split = sansCard |> String.split ":"

    match split with
    | [| id; rest |] ->
        match rest |> String.split "|" with
        | [| winningNumbersStr; numbersStr |] ->
            let winningNumbers = winningNumbersStr |> parseNumbers
            let numbers = numbersStr |> parseNumbers

            { Id = int id
              WinningNumbers = winningNumbers
              Numbers = numbers }
        | other -> failwith $"Invalid input: %A{other}"
    | other -> failwith $"Invalid input: %A{other}"

let scratchCards = lines |> List.map parseLine
let scores = scratchCards |> List.map getWinningScore
let totalScore = scores |> List.sum

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{totalScore}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"

let rec processAllCards (cardInstanceMap: Map<int, int>) (scs: ScratchCard list) =
    match scs with
    | [] -> cardInstanceMap
    | sc :: rest ->
        // How many of them do we have?
        let numInstances = cardInstanceMap[sc.Id]

        // How many cards forward do we get to duplicate?
        let numWinningNumbers = sc |> getCountWinningNumbers

        let winningCopies =
            rest
            |> List.take (min rest.Length numWinningNumbers)

        let cardInstanceMap' =
            winningCopies
            |> List.fold
                (fun ciMap wc ->
                    let curCopiesTotal = ciMap |> Map.find wc.Id
                    let newTotal = curCopiesTotal + numInstances

                    ciMap.Add(wc.Id, newTotal))
                cardInstanceMap

        processAllCards cardInstanceMap' rest

let initialMap =
    scratchCards
    |> Seq.map (fun sc -> sc.Id, 1)
    |> Map.ofSeq

let cardMap = scratchCards |> processAllCards initialMap
let numCards = cardMap |> Map.values |> Seq.sum

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{numCards}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
