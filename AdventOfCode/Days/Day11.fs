namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day11 =
  type Level = int
  type LevelMap = Level [] []
  type Coordinates = int * int

  let THRESHOLD = 9

  let ParseLine (line: string) : Level [] =
    line
    |> Seq.map(fun c -> c.ToString())
    |> Seq.map(Int32.Parse)
    |> Seq.toArray

  let Parse (inputText: string) : LevelMap =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map(ParseLine)

  let DeepClone (input: LevelMap) : LevelMap =
    let (M, N) = (input.Length - 1, input.[0].Length - 1)
    [| for i in 0 .. M -> [| for j in 0 .. N -> input.[i].[j] |] |]

  module FirstChallenge =
    type ActivationCount = int

    let DoStep (levels: LevelMap) : ActivationCount =
      let (M, N) =
        (levels.Length - 1, levels.[0].Length - 1)

      let activated =
        [| for _ in 0 .. M -> [| for _ in 0 .. N -> false |] |]

      let newActivations = Queue<Coordinates>()

      // increase levels
      for i in 0 .. M do
        for j in 0 .. N do
          levels.[i].[j] <- levels.[i].[j] + 1

          if levels.[i].[j] > THRESHOLD then
            newActivations.Enqueue((i, j))
            activated.[i].[j] <- true

      // flash
      while newActivations.Count > 0 do
        let (ii, jj) = newActivations.Dequeue()

        for i in (Math.Max(ii - 1, 0)) .. (Math.Min(ii + 1, M)) do
          for j in (Math.Max(jj - 1, 0)) .. (Math.Min(jj + 1, N)) do
            levels.[i].[j] <- levels.[i].[j] + 1

            if
              not(activated.[i].[j])
              && levels.[i].[j] > THRESHOLD
            then
              newActivations.Enqueue((i, j))
              activated.[i].[j] <- true

      // reset
      for i in 0 .. M do
        for j in 0 .. N do
          if levels.[i].[j] > THRESHOLD then
            levels.[i].[j] <- 0

      // count step activations
      activated
      |> Seq.collect(id)
      |> Seq.filter(id)
      |> Seq.length

    let CountActivations (steps: int) (input: LevelMap) : ActivationCount =
      let levels = DeepClone(input)

      [ 1 .. steps ]
      |> Seq.fold(fun count _ -> count + DoStep(levels)) (0)

  module SecondChallenge =
    let FirstSyncStep (input: LevelMap) : int =
      let levels = DeepClone(input)
      let all = input.Length * input.[0].Length

      (+) 1
      |> Seq.initInfinite
      |> Seq.skipWhile(fun _ -> FirstChallenge.DoStep(levels) <> all)
      |> Seq.head

type Day11(inputText: string) =
  let input = Day11.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day11.FirstChallenge.CountActivations(100)
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      input
      |> Day11.SecondChallenge.FirstSyncStep
      |> fun res -> res.ToString()
