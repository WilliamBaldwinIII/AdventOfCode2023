#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open System.Diagnostics
open Helpers

let lines = Helpers.readFile "12-ex"

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

let getContiguousGroupsOfSprings (line: string) = ()

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: %A{permutations}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
