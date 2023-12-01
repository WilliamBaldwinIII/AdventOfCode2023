module Helpers

open System.IO
open System


let readFile file =
    let filePath = $"./inputs/{file}.txt"
    filePath |> File.ReadAllLines |> List.ofArray

let readExampleFile file =
    let filePath = $"./inputs/{file}-ex.txt"
    filePath |> File.ReadAllLines |> List.ofArray


module String =

    let substringSafe (startIndex: int) (length: int) (str: string) =
        let length = Math.Min(length, str.Length - startIndex)

        str.Substring(startIndex, length)
