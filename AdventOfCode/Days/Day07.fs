namespace AdventOfCode.Days

open System

module private Day07 =
  type Position = int
  type Input = Position []

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(",")
    |> Array.map(Int32.Parse)

  type Cost = int
  type PositionCount = { Position: Position; Count: int }

  let PositionCounts (input: Input) : PositionCount [] =
    input
    |> Seq.countBy(id)
    |> Seq.map(fun (pos, count) -> { Position = pos; Count = count })
    |> Seq.toArray

  let FindOptimalCost (computeCosts: PositionCount [] -> Cost []) (positionCounts: PositionCount []) : Cost =
    let costsLeft = positionCounts |> computeCosts

    let costsRight =
      positionCounts
      |> Array.map
           (fun posCount ->
             { posCount with
                 Position = -posCount.Position })
      |> computeCosts
      |> Array.rev

    let totalCosts =
      costsLeft
      |> Seq.zip(costsRight)
      |> Seq.map(fun (left, right) -> left + right)

    Seq.min(totalCosts)

  module FirstChallenge =
    let CostFunction (left: Position, right: Position) : Cost = Math.Abs(left - right)

    // O(N) :)
    let ComputeCosts (counts: PositionCount []) : Cost [] =
      let counts =
        counts
        |> Array.sortBy(fun posCount -> posCount.Position)

      let N = counts.Length - 1
      let costs = [| for _ in 0 .. N -> 0 |]
      let mutable cumulativeCount = counts.[0].Count

      for i in 1 .. N do
        let deltaCost =
          CostFunction(counts.[i - 1].Position, counts.[i].Position)

        costs.[i] <- costs.[i - 1] + cumulativeCount * deltaCost
        cumulativeCount <- cumulativeCount + counts.[i].Count

      costs

  module SecondChallenge =
    let CostFunction (left: Position, right: Position) : Cost =
      let delta = Math.Abs(left - right)
      (delta * (delta + 1)) / 2

    // O(N^2) :(
    let ComputeCosts (counts: PositionCount []) : Cost [] =
      let counts =
        counts
        |> Array.sortBy(fun posCount -> posCount.Position)

      let minPos = counts.[0].Position
      let maxPos = Array.last(counts).Position

      let costs = [| for _ in minPos .. maxPos -> 0 |]

      let mutable i = 0

      for pos in (minPos + 1) .. maxPos do
        costs.[pos - minPos] <-
          counts.[0..i]
          |> Seq.map
               (fun posCount ->
                 posCount.Count
                 * CostFunction(posCount.Position, pos))
          |> Seq.sum

        if pos >= counts.[i + 1].Position then
          i <- i + 1

      costs

type Day07(inputText: string) =
  let input = Day07.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day07.PositionCounts
      |> Day07.FindOptimalCost(Day07.FirstChallenge.ComputeCosts)
      |> fun cost -> cost.ToString()

    member this.SecondChallenge() =
      input
      |> Day07.PositionCounts
      |> Day07.FindOptimalCost(Day07.SecondChallenge.ComputeCosts)
      |> fun cost -> cost.ToString()
