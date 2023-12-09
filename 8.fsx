﻿#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers

let lines = Helpers.readFile "8"


type Direction =
    | Left
    | Right

module Direction =
    let parse =
        function
        | 'L' -> Left
        | 'R' -> Right
        | other -> failwith $"Invalid direction! {other}"

type Node =
    { Name: string
      Left: string
      Right: string }

module Node =
    let parse (line: string) =
        let name = line.Substring(0, 3)
        let left = line.Substring(7, 3)
        let right = line.Substring(12, 3)

        { Name = name
          Left = left
          Right = right }




let directionsStr, nodesStrs =
    match lines with
    | d :: _ :: n -> d, n
    | other -> failwith $"Unexpected file format! %A{other}"

let directions =
    directionsStr
    |> Seq.map Direction.parse
    |> List.ofSeq

let nodes = nodesStrs |> List.map Node.parse

let nodeMap =
    nodes
    |> List.map (fun n -> n.Name, n)
    |> Map.ofList


let rec loop count curDirections (nodeName: string) =
    //if nodeName = "ZZZ" then
    if nodeName.EndsWith('Z') then
        count
    else
        let node = nodeMap[nodeName]

        let curDirections =
            match curDirections with
            | [] -> directions // reset
            | _ -> curDirections

        let newNodeName, newDirections =
            match curDirections with
            | Left :: rest -> node.Left, rest
            | Right :: rest -> node.Right, rest
            | other -> failwith $"Invalid! {other}"

        let newCount = count + 1L

        loop newCount newDirections newNodeName

let count = loop 0 directions "AAA"

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 1: {count}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"

let startingNodes =
    nodeMap
    |> Map.keys
    |> Seq.filter (fun k -> k.EndsWith 'A')
    |> Seq.toList


let count2s =
    startingNodes
    |> List.map (fun node -> loop 0 directions node)

/// Lowest common multiple much much faster than traversing everything.
let countProduct = count2s |> List.reduce Math.lcm

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"Part 2: {countProduct}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
