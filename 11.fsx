#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open System.Diagnostics
open Helpers

let lines = Helpers.readFile "11-ex"

let grid = array2D lines

let xLength = grid.GetLength 1
let yLength = grid.GetLength 0

let emptyRows =
    seq {
        for i in 0..yLength do
            if grid |> Array2D.getRow i |> Seq.forall ((<>) '#') then
                yield i
    }
    |> Set.ofSeq

let emptyColumns =
    seq {
        for i in 0..xLength do
            if
                grid |> Array2D.getColumn i
                |> Seq.forall ((<>) '#')
            then
                yield i
    }
    |> Set.ofSeq

printfn $"X: {xLength}"
printfn $"Y: {yLength}"

let expandedList =
    seq {
        for x in 0 .. xLength - 1 do
            if emptyRows.Contains x then
                yield
                    seq {
                        for x in 0 .. xLength + emptyColumns.Count - 2 do
                            '.'
                    }

            yield
                seq {
                    yield!
                        seq {
                            for y in 0 .. yLength - 1 do
                                let c = grid[x, y]

                                if emptyColumns.Contains y then
                                    yield! $"{c}."
                                else
                                    yield c
                        }
                }
    }

let expanded = expandedList |> array2D

printfn $"Grid:"
printfn $"%A{grid}"
printfn $""
printfn $"Expanded:"
printfn $"%A{expanded}"
printfn $""


let expandedFlattened =
    expanded
    |> Array2D.mapi (fun x y v ->
        if expanded[x, y] = '#' then
            Some(x, y)
        else
            None)
    |> Array2D.flatten
    |> Seq.choose id
    |> Seq.toList

let allPairs =
    List.allPairs expandedFlattened expandedFlattened
    |> List.filter (fun (a, b) -> a <> b)
    |> List.map (fun (a, b) -> if a > b then a else b)
    |> List.distinct

printfn $"%A{allPairs}"
printfn $""


//printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
//printfn $"Part 1: {1}"
//printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
