namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day12 =
  type Cave =
    | Start
    | End
    | Big of string
    | Small of string

  type Tunnel = Cave * Cave

  type TunnelMap = Map<Cave, Cave []>

  let ParseCave: string -> Cave =
    function
    | "start" -> Start
    | "end" -> End
    | name when name.ToUpper() = name -> Big(name)
    | name -> Small(name)

  let IsBig: Cave -> bool =
    function
    | Big (_) -> true
    | _ -> false

  let ParseLine (line: string) : Tunnel =
    line
    |> fun s -> s.Split("-")
    |> Array.map(ParseCave)
    |> function
      | [| a; b |] -> (a, b)
      | _ -> raise(Exception("invalid input"))

  let BuildTunnelMap (tunnels: Tunnel []) : TunnelMap =
    tunnels
    |> Seq.collect(fun (a, b) -> [ (a, b); (b, a) ])
    |> Seq.groupBy(fst)
    |> Seq.map(fun (a, edges) -> (a, edges |> Seq.map(snd) |> Seq.toArray))
    |> Map

  let Parse (inputText: string) : TunnelMap =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map(ParseLine)
    |> BuildTunnelMap

  type Path = list<Cave>
  type Visited = Set<Cave>
  type Memo = Dictionary<(Visited * bool * Cave), list<Path>>

  let rec FindPathsRec
    (map: TunnelMap)
    (memo: Memo)
    (visited: Visited)
    (allowTwice: bool)
    (current: Cave)
    : list<Path> =
    if memo.ContainsKey((visited, allowTwice, current)) then
      memo.[(visited, allowTwice, current)]
    else
      let NextPaths (visited: Visited, allowTwice: bool) =
        map.[current]
        |> Seq.filter(fun next -> not(visited.Contains(next)))
        |> Seq.collect(fun next -> FindPathsRec(map) (memo) (visited) (allowTwice) (next))
        |> Seq.toList

      let pathsFromCurrent =
        match current with
        | End -> [ [ current ] ]
        | Small (_) ->
          let visitedWithCurrent = visited.Add(current)

          let mutable pathsFromCurrent =
            NextPaths(visitedWithCurrent, allowTwice)

          if allowTwice then
            pathsFromCurrent <-
              pathsFromCurrent
              |> Seq.append(NextPaths(visited, false))
              |> Set
              |> Seq.toList

          pathsFromCurrent
        | _ -> NextPaths(visited, allowTwice)

      let pathsWithCurrent =
        pathsFromCurrent
        |> List.map(fun pathToEnd -> current :: pathToEnd)

      memo.[(visited, allowTwice, current)] <- pathsWithCurrent
      pathsWithCurrent

  let CountPaths (allowTwice: bool) (map: TunnelMap) : int =
    Start
    |> FindPathsRec(map) (Memo()) (Visited([ Start ])) (allowTwice)
    |> Seq.length

type Day12(inputText: string) =
  let input = Day12.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day12.CountPaths(false)
      |> fun s -> s.ToString()

    member this.SecondChallenge() =
      input
      |> Day12.CountPaths(true)
      |> fun s -> s.ToString()
