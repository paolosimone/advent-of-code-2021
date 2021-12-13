namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day13 =
  type X = int
  type Y = int
  type Point = X * Y

  type Fold =
    | Up of Y
    | Left of X

  type Input =
    { Points: HashSet<Point>
      Folds: Fold [] }

  let ParsePoint (point: string) : Point =
    point
    |> fun s -> s.Split(",")
    |> Array.map(Int32.Parse)
    |> function
      | [| x; y |] -> (x, y)
      | _ -> raise(Exception("invalid input"))

  let ParseFold (fold: string) : Fold =
    fold
    |> fun s -> s.Split("=")
    |> function
      | [| axis; value |] when axis.EndsWith("x") -> value |> Int32.Parse |> Left
      | [| axis; value |] when axis.EndsWith("y") -> value |> Int32.Parse |> Up
      | _ -> raise(Exception("invalid input"))

  let Parse (inputText: string) : Input =
    let (points, folds) =
      inputText
      |> fun s -> s.Split(Environment.NewLine + Environment.NewLine)
      |> function
        | [| points; folds |] -> (points.Split(Environment.NewLine), folds.Split(Environment.NewLine))
        | _ -> raise(Exception("invalid input"))

    { Points = points |> Array.map(ParsePoint) |> HashSet
      Folds = folds |> Array.map(ParseFold) }

  let FoldPoint (fold: Fold) ((x, y): Point) : Point =
    match fold with
    | Up (y0) when y > y0 -> (x, 2 * y0 - y)
    | Left (x0) when x > x0 -> (2 * x0 - x, y)
    | _ -> (x, y)

  let ApplyFold (points: HashSet<Point>) (fold: Fold) : HashSet<Point> =
    let nextPoints = HashSet<Point>()

    for point in points do
      point
      |> FoldPoint(fold)
      |> nextPoints.Add
      |> ignore

    nextPoints


  module FirstChallenge =
    let CountPointsAfterSingleFold (input: Input) : int =
      input.Folds.[0]
      |> ApplyFold(input.Points)
      |> Seq.length

  module SecondChallenge =
    let FollowInstructions (input: Input) : HashSet<Point> =
      input.Folds |> Seq.fold(ApplyFold) (input.Points)

    let ToString (points: HashSet<Point>) =
      let xMax = points |> Seq.map(fst) |> Seq.max
      let yMax = points |> Seq.map(snd) |> Seq.max

      let paper =
        [| for _ in 0 .. yMax -> [| for _ in 0 .. xMax -> " " |] |]

      for (x, y) in points do
        paper.[y].[x] <- "#"

      paper
      |> Seq.map(fun chars -> String.Join("", chars))
      |> fun lines -> String.Join(Environment.NewLine, lines)


type Day13(inputText: string) =
  let input = Day13.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day13.FirstChallenge.CountPointsAfterSingleFold
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      input
      |> Day13.SecondChallenge.FollowInstructions
      |> Day13.SecondChallenge.ToString
      // |> printfn "%s"
      |> ignore

      "RGZLBHFP"
