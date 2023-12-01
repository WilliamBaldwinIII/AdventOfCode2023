open Helpers


#load "Helpers.fs"

open System
open Helpers

let fileName = $"1"

let lines = Helpers.readFile fileName

let part1 () =

    let parseFirstAndLastDigit (str: string) =
        let first = str |> Seq.find Char.IsDigit
        let last = str |> Seq.findBack Char.IsDigit
        let num = int $"{first}{last}"

        num

    let nums = lines |> List.map parseFirstAndLastDigit
    let sum = nums |> List.sum

    sum


// Part 2

let numberWords =
    [ "one", 1
      "two", 2
      "three", 3
      "four", 4
      "five", 5
      "six", 6
      "seven", 7
      "eight", 8
      "nine", 9 ]

let isNumberWord (line: string) (index: int) =
    let getNumberWord =
        numberWords
        |> List.choose (fun (numberWord, number) ->
            let substr =
                line
                |> String.substringSafe index numberWord.Length

            if substr = numberWord then
                Some number
            else
                None)

    getNumberWord |> List.tryHead

// Find the index... but how?
let getFirstNumber (line: string) =
    let limit = line.Length - 1

    let rec loop curIndex =
        let curChar = line[curIndex]

        if curChar |> Char.IsDigit then
            int $"{curChar}"
        else
            let num = isNumberWord line curIndex

            match num with
            | Some n -> n
            | None -> loop (curIndex + 1)

    loop 0

let getLastNumber (line: string) =
    let limit = 0

    let rec loop curIndex =
        let curChar = line[curIndex]

        if curChar |> Char.IsDigit then
            int $"{curChar}"
        else
            let num = isNumberWord line curIndex

            match num with
            | Some n -> n
            | None -> loop (curIndex - 1)

    loop (line.Length - 1)

let getFirstAndLastNumber line =
    let first = getFirstNumber line
    let last = getLastNumber line
    let num = int $"{first}{last}"

    num


let allNumbers = lines |> List.map getFirstAndLastNumber
let sumPart2 = allNumbers |> List.sum
