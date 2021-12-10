namespace AdventOfCode.Days

open System
open System.Collections.Generic

open AdventOfCode.Utils

module private Day10 =
  type Bracket = char
  type Chunk = string
  type Input = Chunk []

  let Parse (inputText: string) : Input =
    inputText |> fun s -> s.Split(Environment.NewLine)

  type Pair = Bracket * Bracket

  let pairs: Map<Bracket, Bracket> =
    Map(
      [ ('(', ')')
        ('[', ']')
        ('{', '}')
        ('<', '>') ]
    )

  let (|Opening|Closing|) (bracket: Bracket) =
    if (pairs |> Map.containsKey(bracket)) then
      Opening
    else
      Closing

  let IsMatch ((opening, closing): Pair) : bool = pairs |> Map.find(opening) = closing

  module FirstChallenge =
    let FindCorruption (chunk: Chunk) : option<Bracket> =
      EarlyReturn() {
        let stack = Stack<Bracket>()

        for bracket in chunk do
          match bracket with
          | Opening -> stack.Push(bracket)
          | Closing ->
            if (not(IsMatch(stack.Pop(), bracket))) then
              return bracket
      }

    let Score: Bracket -> int =
      function
      | ')' -> 3
      | ']' -> 57
      | '}' -> 1197
      | '>' -> 25137
      | _ -> raise(Exception("invalid bracket"))

    let TotalSyntaxErrorScore (input: Input) : int =
      input
      |> Seq.map(FindCorruption)
      |> Seq.map(Option.map(Score) >> Option.defaultValue(0))
      |> Seq.sum

  module SecondChallenge =
    let FindClosingSequence (chunk: Chunk) : seq<Bracket> =
      let stack = Stack<Bracket>()

      for bracket in chunk do
        match bracket with
        | Opening -> stack.Push(bracket)
        | Closing -> stack.Pop() |> ignore

      [ for opening in stack -> pairs |> Map.find(opening) ]

    let Score: Bracket -> uint64 =
      function
      | ')' -> 1UL
      | ']' -> 2UL
      | '}' -> 3UL
      | '>' -> 4UL
      | _ -> raise(Exception("invalid bracket"))

    let ComputeScore (sequence: seq<Bracket>) : uint64 =
      sequence
      |> Seq.fold(fun score bracket -> score * 5UL + Score(bracket)) (0UL)

    let MiddleScore (input: Input) : uint64 =
      let scores =
        input
        |> Seq.filter(FirstChallenge.FindCorruption >> Option.isNone)
        |> Seq.map(FindClosingSequence >> ComputeScore)
        |> Seq.sort
        |> Seq.toArray

      scores.[scores.Length / 2]

type Day10(inputText: string) =
  let input = Day10.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day10.FirstChallenge.TotalSyntaxErrorScore
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      input
      |> Day10.SecondChallenge.MiddleScore
      |> fun res -> res.ToString()
