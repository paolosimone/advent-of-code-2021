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

  // could probably use some memoization :/
  let rec FindPathsRec (map: TunnelMap) (visited: HashSet<Cave>) (allowTwice: bool) (path: Path) : Set<Path> =
    let current = List.head(path)

    let NextPaths (allowTwice) =
      map.[current]
      |> Seq.filter(fun next -> not(visited.Contains(next)))
      |> Seq.map(fun next -> FindPathsRec(map) (visited) (allowTwice) (next :: path))
      |> Set.unionMany

    match current with
    | End -> Set([ path ])
    | Small (_) ->
      visited.Add(current) |> ignore
      let mutable pathsFromCurrent = NextPaths(allowTwice)
      visited.Remove(current) |> ignore

      if allowTwice then
        pathsFromCurrent <- pathsFromCurrent + NextPaths(false)

      pathsFromCurrent
    | _ -> NextPaths(allowTwice)

  let CountPaths (allowTwice: bool) (map: TunnelMap) : int =
    let visited = new HashSet<Cave>([ Start ])

    [ Start ]
    |> FindPathsRec(map) (visited) (allowTwice)
    |> Set.count

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
