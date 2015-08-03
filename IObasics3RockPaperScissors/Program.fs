open System
open System.IO

printfn "*************************************************"
printfn "* Welcome to the F# Rock, Paper, Scissors game !*"
printfn "*************************************************"

let random = Random()
let rand = random.NextDouble()

type Choice =
    | Rock
    | Paper
    | Scissors

///Gets string representation of a Choice
let getChoice = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"

///Defines rules for winning and losing
let wins (a : Choice, b : Choice) =
    match a, b with
    | Rock, Scissors -> true //Rock beats Scissors
    | Paper, Rock -> true  //Paper beats Rock
    | Scissors, Paper -> true  //Scissors beats Paper
    | _, _ -> false //Everything else: Lose

//Generates next move for computer based on conditional probability,
//otherwise known as the Principle of Restricted Choice 
let computerMove r p s =
    let total = r + p + s
    let n = rand
    if n < s / total then Rock
    elif n <= (s + r) / total then Paper
    else Scissors

// Gets move chosen by player
let rec playerMove() =
    printfn "Choose 'R' for Rock, 'P' for Paper, 'S' for Scissors, or 'Q' for quit: "
    let choice = Console.ReadLine()
    match choice with
    | "r" | "R" -> Rock
    | "p" | "P" -> Paper
    | "s" | "S" -> Scissors
    | "q" | "Q" -> exit 0 //Exit code for player to have option to quit
    | _ -> 
        printfn "Invalid Entry"
        playerMove()

//Logic of the game program
let rec game (r:float, p:float, s:float) =
    let computer = computerMove r p s
    let player = playerMove()
    Console.WriteLine ("Player: {0} vs Computer {1}", getChoice player, getChoice computer)
    Console.WriteLine (
        if wins (player, computer) then "Player wins!\n"
        elif wins (computer, player) then "Computer wins!\n"
        else "Tie! \n"
    )

    let nextR = if player = Rock then r + 1.0 else r
    let nextP = if player = Paper then p + 1.0 else p
    let nextS = if player = Scissors then s + 1.0 else s
    let sum wins = wins + 1
    let score n =
        [1..n]
        |> List.map sum
        |> List.sum
        |> printfn "%A" 
    game (nextR, nextP, nextS)
game (1.0, 1.0, 1.0)







[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    0 // return an integer exit code

