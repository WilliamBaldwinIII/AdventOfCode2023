open System
open System.IO
open System.Diagnostics


open Helpers

let fileName = $"3"

let lines =
    $"..\..\..\inputs\{fileName}.txt"
    |> File.ReadAllLines
    |> List.ofArray


//let lines = Helpers.readFile fileName

let arr = array2D lines
let xLength = arr |> Array2D.length1
let yLength = arr |> Array2D.length2

let getSurrounding (x, y) =

    let indexes =
        [ x - 1, y - 1
          x - 1, y
          x - 1, y + 1
          x, y - 1
          x, y + 1
          x + 1, y - 1
          x + 1, y
          x + 1, y + 1 ]
        |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x <= xLength && y <= yLength)

    indexes

let isAdjacentToSymbol (x, y) =
    let indexes = getSurrounding (x, y)

    let isAdjacent =
        indexes
        |> List.exists (fun (x, y) ->
            let elem = arr[x, y]
            elem <> '.' && elem |> Char.IsDigit |> not)

    isAdjacent

type Number =
    { Value: int
      Indexes: (int * int) list }

type ParsingNumber =
    { CurrentValue: string
      Indexes: (int * int) list }

module Number =
    let fromParsingNumber (p: ParsingNumber) =
        { Value = int p.CurrentValue
          Indexes = p.Indexes }

let prependOptionToList list option =
    match option with
    | Some v -> v :: list
    | None -> list

// Need to start parsing numbers.
// A number is a continuous string of digits all on the same line.
// We need to keep track of what all indexes the number is on so
// that we can check if any of them is adjacent to a symbol
let rec gatherNumbers (currentNumber: ParsingNumber option) (numbers: Number list) (curX, curY) =

    if curX >= xLength then
        let number =
            currentNumber
            |> Option.map Number.fromParsingNumber

        number |> prependOptionToList numbers

    elif curY >= yLength then
        let number =
            currentNumber
            |> Option.map Number.fromParsingNumber

        let numbers' = number |> prependOptionToList numbers

        gatherNumbers None numbers' (curX + 1, 0)
    else
        let curElem = arr[curX, curY]

        let newCurrentNumber =

            if curElem |> Char.IsDigit then
                let curValChar = Char.ToString(curElem)
                let curIndex = curX, curY

                match currentNumber with
                | Some c ->
                    Some
                        { CurrentValue = c.CurrentValue + curValChar
                          Indexes = curIndex :: c.Indexes }
                | None ->
                    Some
                        { CurrentValue = curValChar
                          Indexes = [ curIndex ] }
            else
                None

        gatherNumbers newCurrentNumber numbers (curX, curY + 1)

let allNumbers = gatherNumbers None [] (0, 0)

let numbersAdjToSymbol =
    allNumbers
    |> List.filter (fun n -> n.Indexes |> List.exists isAdjacentToSymbol)

let sum =
    numbersAdjToSymbol
    |> List.sumBy (fun n -> n.Value)

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{sum}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
