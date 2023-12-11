module Helpers

open System.IO
open System


let readFile file =
    let filePath = $"./inputs/{file}.txt"
    filePath |> File.ReadAllLines |> List.ofArray

let readFileString file =
    let filePath = $"./inputs/{file}.txt"
    filePath |> File.ReadAllText


module String =

    let substringSafe (startIndex: int) (length: int) (str: string) =
        let length = Math.Min(length, str.Length - startIndex)

        str.Substring(startIndex, length)

    let split (delimeter: string) (str: string) =
        str.Split(delimeter, StringSplitOptions.RemoveEmptyEntries)

module List =
    let inline product list =
        list
        |> List.fold (fun total cur -> total * cur) LanguagePrimitives.GenericOne

module Array2D =
    let getSurrounding arr (x, y) =
        let xLength = arr |> Array2D.length1
        let yLength = arr |> Array2D.length2

        let indexes =
            [ x - 1, y - 1
              x - 1, y
              x - 1, y + 1
              x, y - 1
              x, y + 1
              x + 1, y - 1
              x + 1, y
              x + 1, y + 1 ]
            |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < xLength && y < yLength)

        indexes

    let getDirectlySurrounding arr (x, y) =
        let surrounding = getSurrounding arr (x, y)

        let indexes =
            surrounding
            |> List.filter (fun (x', y') -> x' = x || y = y')

        indexes

    let findIndexOf needle (arr: 'a [,]) =
        let rec loop x y =
            if y >= arr.GetLength 1 then
                None
            elif x >= arr.GetLength 0 then
                loop 0 (y + 1)
            elif arr.[x, y] = needle then
                Some(x, y)
            else
                loop (x + 1) y

        loop 0

    // https://stackoverflow.com/a/2369690
    let flatten (A: 'a [,]) = A |> Seq.cast<'a>

    // https://stackoverflow.com/a/2369690
    let getColumn c (A: 'a [,]) = flatten A[*, c..c] |> Seq.toArray

    // https://stackoverflow.com/a/2369690
    let getRow r (A: 'a [,]) = flatten A[r..r, *] |> Seq.toArray

module Math =
    /// Greatest common denominator
    let rec gcd (a: int64) (b: int64) = if b = 0 then a else gcd b (a % b)

    /// Lowest common multiple
    let lcm a b = (a / gcd a b) * b
