#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers


let lines = Helpers.readFile "6-ex"

let timeLine, distanceLine =
    match lines with
    | t :: d :: [] -> t, d
    | _ -> failwith "Expected only 2 lines!"

let times =
    timeLine.Substring("Time:".Length)
    |> String.split " "
    |> Seq.map int64
    |> List.ofSeq

let distances =
    distanceLine.Substring("Distance:".Length)
    |> String.split " "
    |> Seq.map int64
    |> List.ofSeq


let getDistanceForTimeHolding totalTime timeHolding =
    let timeLeft = totalTime - timeHolding
    let totalDistance = timeHolding * timeLeft
    totalDistance

let getWaysToWin (totalTime, recordDistance) =
    let holdDownTimes = [ 0L .. totalTime ]

    let distances =
        holdDownTimes
        |> List.map (getDistanceForTimeHolding totalTime)

    let winningDistances =
        distances
        |> List.filter (fun d -> d > recordDistance)

    winningDistances

let timesAndDistances = List.zip times distances

let answer =
    timesAndDistances
    |> List.map (getWaysToWin >> List.length)
    |> List.product

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: {answer}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"

let time2 =
    timeLine
        .Substring("Time:".Length)
        .Replace(" ", "")
    |> int64

let distance2 =
    distanceLine
        .Substring("Distance:".Length)
        .Replace(" ", "")
    |> int64

let waysToWin2 = getWaysToWin (time2, distance2)


printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 2: {waysToWin2.Length}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
