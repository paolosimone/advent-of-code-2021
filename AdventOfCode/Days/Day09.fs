namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day09 =
  type Height = int
  type HeightMap = Height [] []
  type Coordinates = int * int

  let MAX_HEIGHT = 9

  let ParseLine (line: string) : Height [] =
    line
    |> Seq.map(fun c -> c.ToString())
    |> Seq.map(Int32.Parse)
    |> Seq.toArray

  let AddBorders (map: HeightMap) : HeightMap =
    let (M, N) = (map.Length, map.[0].Length)

    [| yield [| for _ in 0 .. (N + 1) -> MAX_HEIGHT |]
       for i in 0 .. (M - 1) ->
         [| yield MAX_HEIGHT
            for j in 0 .. (N - 1) -> map.[i].[j]
            yield MAX_HEIGHT |]
       yield [| for _ in 0 .. (N + 1) -> MAX_HEIGHT |] |]

  let Parse (inputText: string) : HeightMap =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map(ParseLine)
    |> AddBorders

  module FirstChallenge =
    let IsMinimum (map: HeightMap, (i, j): Coordinates) : bool =
      map.[i].[j] < Math.Min(map.[i - 1].[j], map.[i + 1].[j])
      && map.[i].[j] < Math.Min(map.[i].[j - 1], map.[i].[j + 1])

    let FindMinimums (map: HeightMap) : seq<Coordinates> =
      let (M, N) = (map.Length - 1, map.[0].Length - 1)

      let minimums = List<Coordinates>()

      for i in 1 .. (M - 1) do
        for j in 1 .. (N - 1) do
          if IsMinimum(map, (i, j)) then
            minimums.Add((i, j))

      minimums

    let FindMinimumHeights (map: HeightMap) : seq<Height> =
      map
      |> FindMinimums
      |> Seq.map(fun (i, j) -> map.[i].[j])

  module SecondChallenge =
    type Visited = bool [] []

    let IsNewBasinPoint (map: HeightMap, visited: Visited, (i, j): Coordinates) : bool =
      not(visited.[i].[j]) && map.[i].[j] < MAX_HEIGHT

    let rec BasinSize (map: HeightMap, visited: Visited, (i, j): Coordinates) : int =
      if not(IsNewBasinPoint(map, visited, (i, j))) then
        0
      else
        visited.[i].[j] <- true

        1
        + BasinSize(map, visited, (i - 1, j))
        + BasinSize(map, visited, (i + 1, j))
        + BasinSize(map, visited, (i, j - 1))
        + BasinSize(map, visited, (i, j + 1))

    let FindBasins (map: HeightMap) : seq<int> =
      let (M, N) = (map.Length, map.[0].Length)

      let visited =
        [| for _ in 0 .. M -> [| for _ in 0 .. N -> false |] |]

      let basins = List<int>()

      for (i, j) in FirstChallenge.FindMinimums(map) do
        if (IsNewBasinPoint(map, visited, (i, j))) then
          basins.Add(BasinSize(map, visited, (i, j)))

      basins


type Day09(inputText: string) =
  let input = Day09.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      let minimums =
        Day09.FirstChallenge.FindMinimumHeights(input)

      let result = Seq.sum(minimums) + Seq.length(minimums)
      result.ToString()

    member this.SecondChallenge() =
      input
      |> Day09.SecondChallenge.FindBasins
      |> Seq.sortDescending
      |> Seq.take(3)
      |> Seq.reduce((*))
      |> fun s -> s.ToString()
