#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers

let lines = Helpers.readFile "10-ex"


type Direction =
    | North
    | South
    | East
    | West

module Direction =
    let parse (sourceX, sourceY) (destinationX, destinationY) =
        if sourceX < destinationX && sourceY = destinationY then
            South
        elif sourceX > destinationX && sourceY = destinationY then
            North
        elif sourceX = destinationX && sourceY < destinationY then
            East
        elif sourceX = destinationX && sourceY > destinationY then
            West
        else
            failwith
                $"Invalid direction comparison! Source {(sourceX, sourceY)}; Destination {(destinationX, destinationY)}"


type Tile =
    | Vertical
    | Horizontal
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Ground
    | Starting

module Tile =
    let parse =
        function
        | '|' -> Vertical
        | '-' -> Horizontal
        | 'L' -> NorthEast
        | 'J' -> NorthWest
        | '7' -> SouthWest
        | 'F' -> SouthEast
        | '.' -> Ground
        | 'S' -> Starting
        | other -> failwith $"Invalid character! {other}"

    let doConnect currentTile (destinationTile, currentDirection) =
        match currentTile, currentDirection, destinationTile with
        | Vertical, North, SouthEast
        | Vertical, North, Vertical
        | Vertical, North, SouthWest -> true
        | Vertical, South, NorthEast
        | Vertical, South, Vertical
        | Vertical, South, NorthWest -> true

        | Horizontal, West, NorthEast
        | Horizontal, West, Horizontal
        | Horizontal, West, SouthEast -> true
        | Horizontal, East, NorthWest
        | Horizontal, East, Horizontal
        | Horizontal, East, SouthWest -> true

        | NorthEast, East, NorthWest
        | NorthEast, East, Horizontal
        | NorthEast, East, SouthWest -> true
        | NorthEast, North, SouthWest
        | NorthEast, North, Vertical
        | NorthEast, North, SouthEast -> true

        | SouthEast, East, NorthWest
        | SouthEast, East, Horizontal
        | SouthEast, East, SouthWest -> true
        | SouthEast, South, SouthWest
        | SouthEast, South, Vertical
        | SouthEast, South, SouthEast -> true

        | NorthWest, West, NorthEast
        | NorthWest, West, Horizontal
        | NorthWest, West, SouthEast -> true
        | NorthWest, South, NorthWest
        | NorthWest, South, Vertical
        | NorthWest, South, NorthEast -> true

        | SouthWest, West, NorthEast
        | SouthWest, West, Horizontal
        | SouthWest, West, SouthEast -> true
        | SouthWest, North, NorthWest
        | SouthWest, North, Vertical
        | SouthWest, North, NorthEast -> true

        | Starting, North, SouthEast
        | Starting, North, Vertical
        | Starting, North, SouthWest -> true
        | Starting, South, NorthEast
        | Starting, South, Vertical
        | Starting, South, NorthWest -> true

        | Starting, West, NorthEast
        | Starting, West, Horizontal
        | Starting, West, SouthEast -> true
        | Starting, East, NorthWest
        | Starting, East, Horizontal
        | Starting, East, SouthWest -> true

        | _ -> false



let grid = lines |> array2D |> Array2D.map Tile.parse

let startingIndex =
    grid
    |> Array2D.findIndexOf Starting
    |> Option.defaultWith (fun _ -> failwith "Need to have S index!!!!")


let findConnecting index =
    let surroundingIndexes = index |> Array2D.getDirectlySurrounding grid

    let surrounding =
        surroundingIndexes
        |> List.map (fun (x, y) -> (x, y), (grid[x, y], Direction.parse index (x, y)))

    let x, y = index
    let current = grid[x, y]

    surrounding
    |> List.filter (fun (_, v) -> v |> Tile.doConnect current)
    |> List.map fst

let rec createPipe curIndexes curCount map =
    let connecting = curIndexes |> List.collect findConnecting

    let filteredIndexes =
        connecting
        |> List.filter (fun i -> map |> Map.containsKey i |> not)

    match filteredIndexes with
    | [] ->
        Console.WriteLine($"No more indexes. Returning. Count: {curCount}")
        map
    | filteredIndexes ->
        Console.WriteLine($"Still going. Count: {curCount}")
        let newCount = curCount + 1

        let map' =
            filteredIndexes
            |> List.fold (fun (curMap: Map<(int * int), int>) b -> curMap.Add(b, newCount)) map

        let newIndexes =
            filteredIndexes
            |> List.collect (Array2D.getDirectlySurrounding grid)

        createPipe newIndexes newCount map'

let pipeMap = createPipe [ startingIndex ] 0 Map.empty
let pipeMaxDistance = pipeMap |> Map.values |> Seq.max

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: {pipeMaxDistance}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
