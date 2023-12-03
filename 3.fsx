#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers

let fileName = $"3"

let lines = Helpers.readFile fileName

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
        |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < xLength && y < yLength)

    indexes

let isAdjacentToSymbol (x, y) =
    let indexes = getSurrounding (x, y)

    let isAdjacent =
        indexes
        |> List.exists (fun (x, y) ->
            let elem = arr[x, y]
            elem <> '.' && elem |> Char.IsDigit |> not)

    isAdjacent

let isAdjacentToGear (x, y) =
    let indexes = getSurrounding (x, y)

    let isAdjacent =
        indexes
        |> List.choose (fun (x, y) ->
            let elem = arr[x, y]
            if elem = '*' then Some(x, y) else None)

    isAdjacent |> List.tryHead

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

        let numbers' =
            match newCurrentNumber with
            | Some _ -> numbers
            | None ->
                let number =
                    currentNumber
                    |> Option.map Number.fromParsingNumber

                let numbers' = number |> prependOptionToList numbers
                numbers'

        gatherNumbers newCurrentNumber numbers' (curX, curY + 1)

let allNumbers = gatherNumbers None [] (0, 0)

let numbersAdjToSymbol =
    allNumbers
    |> List.filter (fun n -> n.Indexes |> List.exists isAdjacentToSymbol)

let sum =
    numbersAdjToSymbol
    |> List.sumBy (fun n -> n.Value)

let numbersAdjToGear =
    numbersAdjToSymbol
    |> List.choose (fun n ->
        n.Indexes
        |> List.choose (fun i ->
            match isAdjacentToGear i with
            | Some gear -> Some(gear, n)
            | None -> None)
        |> List.tryHead)

let sumNumbersAdjToGear =
    numbersAdjToGear
    |> List.groupBy fst
    |> List.filter (fun (k, v) -> v.Length > 1)
    |> List.map (fun (k, v) -> k, v |> List.map snd)
    |> List.map (fun (_, n) -> n |> List.map (fun n' -> n'.Value) |> List.product)
    |> List.sum

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{sum}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{sumNumbersAdjToGear}"
printfn "!!!!!!!!!!!!!!!!\n\n\n\n\n\n"
