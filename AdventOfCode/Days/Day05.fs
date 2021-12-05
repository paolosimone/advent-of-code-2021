namespace AdventOfCode.Days

open System

// Naive again, dishonor on me and on my cow!
module private Day05 =
  type X = int
  type Y = int
  type Point = X * Y
  type Line = Point * Point
  type Input = Line []

  let ParsePoint (coords: string) : Point =
    coords
    |> fun s -> s.Split(",")
    |> Array.map(Int32.Parse)
    |> function
      | [| x; y |] -> (x, y)
      | _ -> raise(new Exception("invalid input"))

  let ParseLine (line: string) : Line =
    line
    |> fun s -> s.Split(" -> ")
    |> Array.map(ParsePoint)
    |> function
      | [| a; b |] -> if a < b then (a, b) else (b, a)
      | _ -> raise(new Exception("invalid input"))

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map(ParseLine)


  type Field(input: Input) =
    let (xMax, yMax): Point =
      Seq.fold
        (fun ((xMax, yMax): Point) (((_, y1), (x2, y2)): Line) ->
          (Math.Max(xMax, x2), Math.Max(yMax, Math.Max(y1, y2))))
        (0, 0)
        (input)

    let mutable field =
      [| for _ in 0 .. xMax -> [| for _ in 0 .. yMax -> 0 |] |]

    member this.Add(x: X, y: Y) = field.[x].[y] <- field.[x].[y] + 1

    member this.CountOverlapping() : int =
      let mutable count = 0

      for x in 0 .. xMax do
        for y in 0 .. yMax do
          if field.[x].[y] > 1 then
            count <- count + 1

      count

  let (|Vertical|Horizontal|Diagonal|) (line: Line) =
    match line with
    | ((x1, _), (x2, _)) when x1 = x2 -> Vertical
    | ((_, y1), (_, y2)) when y1 = y2 -> Horizontal
    | _ -> Diagonal


  module FirstChallenge =
    let CountOverlapping (input: Input) : int =
      let field = Field(input)

      for line in input do
        let ((x1, y1), (x2, y2)) = line

        match line with
        | Vertical ->
          for y in y1 .. y2 do
            field.Add(x1, y)
        | Horizontal ->
          for x in x1 .. x2 do
            field.Add(x, y1)
        | _ -> ()

      field.CountOverlapping()


  module SecondChallenge =
    let CountOverlapping (input: Input) : int =
      let field = Field(input)

      for line in input do
        let ((x1, y1), (x2, y2)) = line

        match line with
        | Vertical ->
          for y in y1 .. y2 do
            field.Add(x1, y)
        | Horizontal ->
          for x in x1 .. x2 do
            field.Add(x, y1)
        | Diagonal ->
          let deltaY = if y2 > y1 then 1 else -1
          let mutable y = y1

          for x in x1 .. x2 do
            field.Add(x, y)
            y <- y + deltaY

      field.CountOverlapping()


type Day05(inputText: string) =
  let input = Day05.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day05.FirstChallenge.CountOverlapping
      |> fun count -> count.ToString()

    member this.SecondChallenge() =
      input
      |> Day05.SecondChallenge.CountOverlapping
      |> fun count -> count.ToString()
