open System.Numerics


#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers


type Range = { Start: int64; End: int64 }

type RangeCombo = { Source: Range; Destination: Range }

let parseLine line =
    let numbers = line |> String.split " " |> Array.map int64

    match numbers with
    | [| destinationRangeStart; sourceRangeStart; rangeLength |] ->
        let sourceRange =
            { Start = sourceRangeStart
              End = sourceRangeStart + rangeLength }

        let destinationRange =
            { Start = destinationRangeStart
              End = destinationRangeStart + rangeLength }

        { Source = sourceRange
          Destination = destinationRange }

    | _ -> failwith $"Expected exactly 3 elements: %A{numbers}"

let parseMap linesChunk =
    linesChunk
    |> String.split "\n"
    |> List.ofArray
    |> List.tail
    |> List.map parseLine

let lookupIndex index map =
    let ourRange =
        map
        |> List.filter (fun (r: RangeCombo) ->
            let source = r.Source
            source.Start <= index && source.End >= index)
        |> List.tryExactlyOne

    match ourRange with
    | None -> index
    | Some range ->
        let increment = index - range.Source.Start
        let destination = range.Destination.Start + increment

        destination

let fileName = $"5"

let fileString = Helpers.readFileString fileName

let chunks =
    fileString.Replace("\r", "")
    |> String.split "\n\n"
    |> List.ofArray

let seedLine, rest =
    match chunks with
    | s :: r -> s, r
    | other -> failwith $"Invalid parsed file: %A{other}"


let maps = rest |> List.map parseMap


let seedNumbers =
    let seedsStr = "seeds: "

    seedLine.Substring seedsStr.Length
    |> String.split " "
    |> Seq.map int64
    |> List.ofSeq

let getLocation (seedNumber: int64) =
    maps |> List.fold lookupIndex seedNumber

let lowestLocation = seedNumbers |> List.map getLocation |> List.min

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: {lowestLocation}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
