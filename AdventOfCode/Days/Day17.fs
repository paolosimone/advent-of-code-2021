namespace AdventOfCode.Days

open System
open System.Collections.Generic

// Ok, this one took me longer than expected and during the whole process
// I had vividly in mind the disappointed face of my high school math teacher
module private Day17 =
  type Position = int
  type Velocity = int
  type Time = int
  type TargetCondition = Velocity * Time

  type Range = Position * Position
  type TargetArea = Range * Range

  let ParseRange (range: string) : Range =
    range.[2..]
    |> fun s -> s.Split("..")
    |> Array.map(int)
    |> function
      | [| start; stop |] -> (start, stop)
      | _ -> raise(Exception("invalid input"))

  let Parse (inputText: string) : TargetArea =
    inputText
    |> fun s -> s.Split("|")
    |> Array.map(ParseRange)
    |> function
      | [| x; y |] -> (x, y)
      | _ -> raise(Exception("invalid input"))

  let Inside ((start, stop): Range) (pos: Position) : bool = start <= pos && pos <= stop

  module Horizontal =
    // The motion stops after v time steps.
    // The final x is v + (v-1) + ... + 1 = v*(v+1)/2
    // To know x after t steps we must subtract from the whole space the remaining (v-t) steps
    let X (v: Velocity, t: Time) : Position =
      if t >= v then
        v * (v + 1) / 2
      else
        v * (v + 1) / 2 - (v - t) * (v - t + 1) / 2

    // The minumum starting velocity to stop after x is obtained
    // by solving the equation v*(v+1)/2 = x for positive v
    // -> v^2 + v - 2x = 0
    // -> v = ceil((sqrt(1 + 8x) - 1 ) / 2)
    let MinVToReachX (x: Position) : Velocity =
      int(Math.Ceiling((Math.Sqrt(8.0 * double(x) + 1.0) - 1.0) / 2.0))

    // Find all starting velocities that reach the target area
    // and the related time of arrival.
    // If a velocity make the x stop exactly in the target area
    // then it will stay there forever
    type HorizontalTargetConditions =
      { Pass: List<TargetCondition>
        Forever: List<TargetCondition> }

    let Solve ((start, stop): Range) : HorizontalTargetConditions =
      let inTarget = Inside((start, stop))

      let res =
        { Pass = List<TargetCondition>()
          Forever = List<TargetCondition>() }

      // the slowest way to reach the target is by stopping exactly at start
      let vMin = MinVToReachX(start)
      let mutable tMax = vMin

      // the maximum velocity is the one that reach stop in 1 step
      for v in vMin .. stop do
        // skip overshooting
        while X(v, tMax) > stop do
          tMax <- tMax - 1

        // find all times in which we pass through the target area
        let mutable t = tMax

        while inTarget(X(v, t)) do
          let list =
            if inTarget(X(v, v)) then
              res.Forever
            else
              res.Pass

          list.Add((v, t))
          t <- t - 1

      res

    // Resolve all infinite time solutions up to a max time.
    // Probably there is a smarter way to count solutions that doesn't involve this step.
    let Explode (tMax: Time) (solutions: HorizontalTargetConditions) : seq<TargetCondition> =
      let res = List<TargetCondition>(solutions.Pass)

      for (v, tMin) in solutions.Forever do
        for t in tMin .. (Math.Max(tMin, tMax)) do
          res.Add((v, t))

      res

  module Vertical =
    // Note: the y axis is inverted wrt problem definition
    let Simulate (v: Velocity) : seq<Position> =
      seq {
        let mutable (y, v) = (0, v)

        while true do
          yield y
          y <- y + v
          v <- v + 1
      }

    // t:      0  1  2  3  4  5
    // -------------------------
    // y(v=0): 0  0  1  3  6  10
    // y(v=1): 0  1  3  6  10 15
    // y(v=2): 0  2  5  9  14 20
    // ...
    let Y (gravity: Position []) (v: Velocity, t: Time) : Position = gravity.[t] + v * t

    // Find all starting velocities that reach the target area
    // and the related time of arrival.
    let Solve ((start, stop): Range) : seq<TargetCondition> =
      // the slowest way to reach the target (without shooting up) is to let gravity do is job
      let gravity =
        Simulate(0)
        |> Seq.takeWhile(fun y -> y <= stop)
        |> Seq.toArray

      let mutable tMax = gravity.Length - 1
      let Y = Y(gravity)

      let inTarget = Inside((start, stop))
      let res = List<TargetCondition>()

      // the maximum velocity is the one that reach stop in 1 step.
      // v=0 is considered as shooting up from v=1
      for v in 1 .. stop do
        // skip overshooting
        while Y(v, tMax) > stop do
          tMax <- tMax - 1

        let mutable t = tMax

        while inTarget(Y(v, t)) do
          res.Add((v, t))
          // we can reach the target in the same way also by shooting up:
          // start at y=0 with up(v-1) -> ...after t+2*v-1 steps... -> y=0 with down(v)
          res.Add((-v + 1, t + 2 * v - 1))
          t <- t - 1

      res

  let GroupByTime (solutions: seq<TargetCondition>) : Map<Time, seq<Velocity>> =
    solutions
    |> Seq.groupBy(snd)
    |> Seq.map(fun (t, conditions) -> (t, conditions |> Seq.map(fst)))
    |> Map

  let CountAllSolutions ((xRange, (start, stop)): TargetArea) =
    let yRange = (-stop, -start)
    let ySol = Vertical.Solve(yRange) |> GroupByTime
    let tMax = ySol |> Map.keys |> Seq.max

    let xSol =
      Horizontal.Solve(xRange)
      |> Horizontal.Explode(tMax)
      |> GroupByTime

    // need a set to prevent double counting same starting conditions
    // that pass through the target area more than once
    let res = HashSet<Velocity * Velocity>()

    for KeyValue (t, yVs) in ySol do
      match xSol.TryFind(t) with
      | Some (xVs) ->
        for (vx, vy) in Seq.allPairs(xVs) (yVs) do
          res.Add((vx, -vy)) |> ignore
      | _ -> ()

    res.Count

type Day17(inputText: string) =
  let input = Day17.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      // the higher they rise, the harder they fall
      let (_, (yBottom, _)) = input
      let velocityUp = yBottom + 1
      let highestStep = Math.Abs(yBottom)

      velocityUp
      |> Day17.Vertical.Simulate
      |> Seq.skip(highestStep - 1)
      |> Seq.head
      |> Math.Abs
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      input
      |> Day17.CountAllSolutions
      |> fun res -> res.ToString()
