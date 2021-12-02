namespace AdventOfCode.Days

open System

module private Day02 =
  type Command =
    | Forward of int
    | Down of int
    | Up of int

  type Input = List<Command>

  let ParseRow (s: string) =
    let (command, number) =
      match s.Split(" ") with
      | [| command; number |] -> (command, Int32.Parse(number))
      | _ -> raise(new Exception("invalid input"))

    match command with
    | "forward" -> Forward number
    | "down" -> Down number
    | "up" -> Up number
    | _ -> raise(new Exception("invalid input"))

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Seq.map(ParseRow)
    |> Seq.toList

  module FirstChallenge =
    type Position = (int * int)

    let ApplyCommand ((x, y): Position) (command: Command) : Position =
      match command with
      | Forward delta -> (x + delta, y)
      | Down delta -> (x, y + delta)
      | Up delta -> (x, y - delta)

    let FollowDirections (input: Input) : Position = input |> Seq.fold(ApplyCommand) (0, 0)

  module SecondChallenge =
    type Position = (int * int * int)

    let ApplyCommand ((x, y, aim): Position) (command: Command) : Position =
      match command with
      | Forward delta -> (x + delta, y + aim * delta, aim)
      | Down delta -> (x, y, aim + delta)
      | Up delta -> (x, y, aim - delta)

    let FollowDirections (input: Input) : Position =
      input |> Seq.fold(ApplyCommand) (0, 0, 0)

type Day02(inputText: string) =
  let input = Day02.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day02.FirstChallenge.FollowDirections
      |> fun (x, y) -> (x * y).ToString()

    member this.SecondChallenge() =
      input
      |> Day02.SecondChallenge.FollowDirections
      |> fun (x, y, _) -> (x * y).ToString()
