namespace AdventOfCode.Days

open System

module private Day06 =
  type Input = int []

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(",")
    |> Array.map(Int32.Parse)

  type Population(input: Input) =
    let EmptyTimers () = [| for _ in 0 .. 8 -> 0UL |]

    let mutable timers = EmptyTimers()

    do
      for timer in input do
        timers.[timer] <- timers.[timer] + 1UL

    member this.NextDay() =
      let newTimers = EmptyTimers()

      // decrease the timers
      newTimers.[0..7] <- timers.[1..8]

      // give birth
      newTimers.[8] <- timers.[0]
      newTimers.[6] <- newTimers.[6] + timers.[0]

      timers <- newTimers

    member this.Count() = Array.sum(timers)

  let simulation (input: Input) =
    seq {
      let population = Population(input)

      while true do
        population.NextDay()
        yield population.Count()
    }

type Day06(inputText: string) =
  let input = Day06.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day06.simulation
      |> Seq.skip(79)
      |> Seq.head
      |> fun count -> count.ToString()

    member this.SecondChallenge() =
      input
      |> Day06.simulation
      |> Seq.skip(255)
      |> Seq.head
      |> fun count -> count.ToString()
