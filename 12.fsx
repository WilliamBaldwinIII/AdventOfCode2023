#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open System.Diagnostics
open Helpers

let lines = Helpers.readFile "12"

type Spot =
    | Spring
    | Not
    | Question

module Spot =
    let parse =
        function
        | '#' -> Spring
        | '.' -> Not
        | '?' -> Question
        | other -> failwith $"Invalid character: {other}"

let parseLine (line: string) =
    match line |> String.split " " with
    | [| conditionRecord; contiguousRecord |] ->
        let conditions =
            conditionRecord
            |> Seq.map Spot.parse
            |> Seq.mapi (fun i s -> i, s)
            |> Seq.toList

        let contiguous =
            contiguousRecord
            |> String.split ","
            |> Seq.map int
            |> Seq.toList

        conditions, contiguous

    | other -> failwith $"Invalid number of elements! {other}"

let rec getBooleanPermutations (listSoFar: (int * bool) list) (questionMarks: int list) =
    match questionMarks with
    | [] -> [ listSoFar ]
    | index :: xs ->
        let truePath = (index, true) :: listSoFar
        let falsePath = (index, false) :: listSoFar

        let list1 = getBooleanPermutations truePath xs
        let list2 = getBooleanPermutations falsePath xs

        list1 @ list2


let parsedLines = lines |> List.map parseLine
let spots, contiguous = parsedLines |> List.head

let permutations =
    spots
    |> List.filter (fun (_, s) -> s = Question)
    |> List.map fst
    |> getBooleanPermutations []

let rec getContiguousGroupsOfSprings
    (currentContigCount: int option)
    (contigs: int list)
    (spots: (int * Spot) list)
    (map: Map<int, bool>)
    =
    match spots with
    | [] ->
        let newContigList =
            match currentContigCount with
            | Some count -> count :: contigs
            | None -> contigs
            |> List.rev

        newContigList
    | (index, spot) :: rest ->
        let isSpring =
            match spot with
            | Question -> map[index]
            | Spring -> true
            | Not -> false

        let newCount, newContigList =
            match isSpring, currentContigCount with
            | true, Some count -> Some(count + 1), contigs
            | true, None -> Some 1, contigs
            | false, Some count -> None, count :: contigs
            | false, None -> None, contigs

        getContiguousGroupsOfSprings newCount newContigList rest map

let runLine (spots: (int * Spot) list, contiguous: int list) =
    let permutations =
        spots
        |> List.filter (fun (_, s) -> s = Question)
        |> List.map fst
        |> getBooleanPermutations []

    let permutationMaps = permutations |> List.map Map.ofList

    let results =
        permutationMaps
        |> List.map (getContiguousGroupsOfSprings None [] spots)

    let matching = results |> List.filter ((=) contiguous)

    matching |> List.length

let fullResults = parsedLines |> List.map runLine

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: %A{fullResults |> List.sum}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
