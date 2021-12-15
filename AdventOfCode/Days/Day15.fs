namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day15 =
  type Risk = int
  type RiskTile = Risk [] []
  type Coordinates = int * int

  type RiskMap(tile: RiskTile, replication: int) =
    let (M, N) = (tile.Length, tile.[0].Length)

    member val Height = M * replication
    member val Width = N * replication

    member this.Item((i, j): Coordinates) : Risk =
      let risk = tile.[i % M].[j % N] + i / M + j / N
      risk % 10 + risk / 10

  let ParseLine (line: string) : Risk [] =
    line
    |> Seq.map(fun c -> c.ToString())
    |> Seq.map(Int32.Parse)
    |> Seq.toArray

  let Parse (inputText: string) : RiskTile =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map(ParseLine)

  // My journey with the Force
  //
  // # Padawan
  // my solution used DP, was extremely fast and worked with test input
  // ...but was suboptimal with real input of second challenge (3019) :/
  //
  // # Sith
  // so I brute-force submitted lower result until I reached the optimal solution (3012)
  //
  // # Redemption
  // I came back and implemented an actual optimal algorithm (~Dijkstra)
  //
  let FindBestPath (map: RiskMap) : Risk =
    let (M, N) = (map.Height - 1, map.Width - 1)

    let bestPath: option<int> [] [] =
      [| for _ in 0 .. M -> [| for _ in 0 .. N -> None |] |]

    let frontier =
      PriorityQueue<(Coordinates * Risk), Risk>()

    frontier.Enqueue(((0, 0), 0), 0)

    let MaybeEnqueue ((i, j): Coordinates, fromRisk: Risk) =
      if i >= 0
         && i <= M
         && j >= 0
         && j <= N
         && bestPath.[i].[j].IsNone then
        let risk = fromRisk + map.[(i, j)]
        frontier.Enqueue(((i, j), risk), risk)

    while frontier.Count > 0 do
      let ((i, j), risk) = frontier.Dequeue()

      if bestPath.[i].[j].IsNone then
        bestPath.[i].[j] <- Some(risk)
        MaybeEnqueue((i - 1, j), risk)
        MaybeEnqueue((i + 1, j), risk)
        MaybeEnqueue((i, j - 1), risk)
        MaybeEnqueue((i, j + 1), risk)

    bestPath.[M].[N].Value


type Day15(inputText: string) =
  let input = Day15.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      Day15.RiskMap(input, 1)
      |> Day15.FindBestPath
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      Day15.RiskMap(input, 5)
      |> Day15.FindBestPath
      |> fun res -> res.ToString()
