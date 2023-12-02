#load "Helpers.fs"

fsi.ShowProperties <- false
fsi.ShowIEnumerable <- false
fsi.ShowDeclarationValues <- false

open System
open Helpers

let fileName = $"2"

let lines = Helpers.readFile fileName

type Game =
    { Id: int
      MaxGreen: int
      MaxRed: int
      MaxBlue: int }


type Color =
    | Red
    | Green
    | Blue

module Color =
    let parse (colorStr: string) =
        let trimmed = colorStr.Trim().ToLower()

        match trimmed with
        | "red" -> Red
        | "blue" -> Blue
        | "green" -> Green
        | other -> failwith $"{other} is not a valid color!"

type ColorRound = { Color: Color; NumDice: int }

let gameLength = "Game ".Length

let parseGameNumberAndGetRest (line: string) =
    let sansGame = line.Substring gameLength
    let split = sansGame.Split(':', StringSplitOptions.RemoveEmptyEntries)

    match split with
    | [| gameNumber; rest |] -> int gameNumber, rest
    | _ -> failwith $"Unexpected number of elements! %A{split}"

let parseRounds (parsedLine: string) = parsedLine.Split ';'

let parseColor (color: string) =
    let split = color.Split(' ', StringSplitOptions.RemoveEmptyEntries)

    match split with
    | [| number; color |] ->
        { Color = Color.parse color
          NumDice = int number }
    | _ -> failwith $"Invalid number of elements for color: %s{color}"


let parseRound (round: string) =
    let colors = round.Split(',', StringSplitOptions.RemoveEmptyEntries)
    let parsedColors = colors |> Array.map parseColor
    parsedColors

let parseGame (line: string) =
    let gameNumber, rest = parseGameNumberAndGetRest line
    let rounds = rest |> parseRounds |> Seq.collect parseRound

    let groupedByColor =
        rounds
        |> Seq.groupBy (fun r -> r.Color)
        |> Map.ofSeq

    let getMaxForColor color =
        let max =
            groupedByColor[color]
            |> Seq.maxBy (fun cr -> cr.NumDice)

        max.NumDice

    let game =
        { Id = gameNumber
          MaxBlue = getMaxForColor Blue
          MaxRed = getMaxForColor Red
          MaxGreen = getMaxForColor Green }

    game

let actualDice =
    { Id = -1
      MaxRed = 12
      MaxGreen = 13
      MaxBlue = 14 }

let isValidGame (game: Game) =
    game.MaxRed <= actualDice.MaxRed
    && game.MaxGreen <= actualDice.MaxGreen
    && game.MaxBlue <= actualDice.MaxBlue

let allGames = lines |> List.map parseGame

let validGames =
    allGames
    |> List.filter isValidGame
    |> List.sumBy (fun game -> game.Id)

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{validGames}"
printfn "!!!!!!!!!!!!!!!!"


let getPower (game: Game) =
    game.MaxBlue * game.MaxRed * game.MaxGreen

let powerSum = allGames |> List.sumBy getPower

printfn "\n\n\n\n\n\n!!!!!!!!!!!!!!!!"
printfn $"{powerSum}"
printfn "!!!!!!!!!!!!!!!!"
