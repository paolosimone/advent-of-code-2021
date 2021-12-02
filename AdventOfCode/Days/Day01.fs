namespace AdventOfCode.Days

open System

module private Day01 =
  type Input = List<int>

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Seq.map(Int32.Parse)
    |> Seq.toList

  let CountIncrements (input: Input) : int =
    input.Tail
    |> Seq.zip(input)
    |> Seq.filter(fun (prev, next) -> next > prev)
    |> Seq.length

  let CountIncrementWindows (input: Input) : int =
    input
    |> Seq.windowed(3)
    |> Seq.map(Array.sum)
    |> Seq.toList
    |> CountIncrements

type Day01(inputText: string) =
  let input = Day01.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day01.CountIncrements
      |> fun count -> count.ToString()

    member this.SecondChallenge() =
      input
      |> Day01.CountIncrementWindows
      |> fun count -> count.ToString()
