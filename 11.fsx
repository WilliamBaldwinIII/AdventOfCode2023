#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open System.Diagnostics
open Helpers

let lines = Helpers.readFile "11"

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


let starCoordinates =
    grid
    |> Array2D.mapi (fun x y v ->
        if grid[x, y] = '#' then
            Some(x, y)
        else
            None)
    |> Array2D.flatten
    |> Seq.choose id
    |> Seq.toList

let allPairs =
    List.allPairs starCoordinates starCoordinates
    |> List.filter (fun (a, b) -> a <> b)
    |> List.map (fun (a, b) -> if a < b then a, b else b, a)
    |> List.distinct


let multiplier = 1

let distances multiplier =
    allPairs
    |> List.map (fun (a, b) ->
        let initialDistance = Math.distance a b |> int64

        let ax, ay = a
        let bx, by = b

        let numEmptyRows =
            emptyRows
            |> Seq.filter (fun c -> (c > ax && c < bx) || (c > bx && c < ax))
            |> Seq.length
            |> int64

        let numEmptyColumns =
            emptyColumns
            |> Seq.filter (fun r -> (r > ay && r < by) || (r > by && r < ay))
            |> Seq.length
            |> int64

        initialDistance
        + (numEmptyColumns * (multiplier - 1L))
        + (numEmptyRows * (multiplier - 1L))
    //- 3


    )

let sum1 = distances 2L |> List.sum
let sum2 = distances 10L |> List.sum
let sum3 = distances 100L |> List.sum
let sum4 = distances 1_000_000L |> List.sum

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: {sum1}"
printfn $"Part 2-ex-1: {sum2}"
printfn $"Part 2-ex-2: {sum3}"
printfn $"Part 2: {sum4}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
