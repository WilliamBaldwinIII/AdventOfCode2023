#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers

let lines = Helpers.readFile "9"

let rec loop list =
    if list |> List.forall ((=) 0L) then
        0L
    else
        let newList =
            list
            |> List.pairwise
            |> List.map (fun (a, b) -> a - b)

        let nextNumberDown = loop newList
        let head = list |> List.head

        head + nextNumberDown


let linesParsed2 =
    lines
    |> List.map (String.split " " >> Array.toList >> List.map int64)

let linesParsed = linesParsed2 |> List.map List.rev

let runPart lines = lines |> List.sumBy loop

let part1 = runPart linesParsed
let part2 = runPart linesParsed2

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: {part1}"
printfn $"Part 2: {part2}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
