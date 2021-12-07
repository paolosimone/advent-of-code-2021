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
  type PositionCount = (Position * int)

  let PositionCounts (input: Input) : PositionCount [] = input |> Seq.countBy(id) |> Seq.toArray

  // O(N * logN) :)
  module FirstChallenge =
    let FuelCost (left: Position, right: Position) : Cost = Math.Abs(left - right)

    let ComputeCosts (posCounts: PositionCount []) : Cost [] =
      let posCounts =
        posCounts |> Array.sortBy(fun (pos, _) -> pos)

      let N = posCounts.Length - 1
      let costs = [| for _ in 0 .. N -> 0 |]
      let mutable cumulativeCount = snd(posCounts.[0])

      for i in 1 .. N do
        let deltaCost =
          FuelCost(fst(posCounts.[i - 1]), fst(posCounts.[i]))

        costs.[i] <- costs.[i - 1] + cumulativeCount * deltaCost
        cumulativeCount <- cumulativeCount + snd(posCounts.[i])

      costs

    let FindOptimalCost (posCounts: PositionCount []) : Cost =
      let costsLeft = posCounts |> ComputeCosts

      let costsRight =
        posCounts
        |> Array.map(fun (pos, count) -> (-pos, count))
        |> ComputeCosts
        |> Array.rev

      let totalCosts =
        costsLeft
        |> Seq.zip(costsRight)
        |> Seq.map(fun (left, right) -> left + right)

      Seq.min(totalCosts)

  // O(N * M) :(
  // ...luckily position range is small enough
  module SecondChallenge =
    let FuelCost (left: Position, right: Position) : Cost =
      let delta = Math.Abs(left - right)
      (delta * (delta + 1)) / 2

    let ComputePositionCost (posCounts: PositionCount [], position: Position) : Cost =
      posCounts
      |> Seq.map(fun (current, count) -> count * FuelCost(current, position))
      |> Seq.sum

    let FindOptimalCost (posCounts: PositionCount []) : Cost =
      let positions =
        posCounts |> Seq.map(fun (pos, _) -> pos)

      let minPos = positions |> Seq.min
      let maxPos = positions |> Seq.max

      let costs =
        [| for pos in minPos .. maxPos -> ComputePositionCost(posCounts, pos) |]

      Seq.min(costs)

type Day07(inputText: string) =
  let input = Day07.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day07.PositionCounts
      |> Day07.FirstChallenge.FindOptimalCost
      |> fun cost -> cost.ToString()

    member this.SecondChallenge() =
      input
      |> Day07.PositionCounts
      |> Day07.SecondChallenge.FindOptimalCost
      |> fun cost -> cost.ToString()
