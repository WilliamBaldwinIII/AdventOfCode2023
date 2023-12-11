open System
open System.IO
open System.Diagnostics


open Helpers

let fileName = $"10-ex-2"

let lines =
    $"..\..\..\inputs\{fileName}.txt"
    |> File.ReadAllLines
    |> List.ofArray
