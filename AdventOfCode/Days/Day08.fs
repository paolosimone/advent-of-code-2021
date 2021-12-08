namespace AdventOfCode.Days

open System

module private Day08 =
  type Signal = char
  type Digit = Set<Signal>
  type Display = Digit [] * Digit []
  type Input = Display []

  let ParseLine (line: string) : Display =
    line
    |> fun s -> s.Split(" | ")
    |> Array.map(fun s -> s.Split(" ") |> Array.map(Set))
    |> function
      | [| signals; digits |] -> (signals, digits)
      | _ -> raise(Exception("Invalid input"))

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map(ParseLine)


  module FirstChallenge =
    let CountUniquePattern (input: Input) : int =
      let commonPatternLenghts = [ 5; 6 ]

      input
      |> Seq.collect(fun (_, digits) -> digits)
      |> Seq.filter
           (fun digits ->
             not(
               commonPatternLenghts
               |> List.contains(digits.Count)
             ))
      |> Seq.length

  module SecondChallenge =
    //  0000
    // 1    2
    // 1    2
    //  3333
    // 4    5
    // 4    5
    //  6666
    type Segments = bool []
    type Wiring = Signal []

    let ReadSegments: Segments -> int =
      function
      | [| true; true; true; false; true; true; true |] -> 0
      | [| false; false; true; false; false; true; false |] -> 1
      | [| true; false; true; true; true; false; true |] -> 2
      | [| true; false; true; true; false; true; true |] -> 3
      | [| false; true; true; true; false; true; false |] -> 4
      | [| true; true; false; true; false; true; true |] -> 5
      | [| true; true; false; true; true; true; true |] -> 6
      | [| true; false; true; false; false; true; false |] -> 7
      | [| true; true; true; true; true; true; true |] -> 8
      | [| true; true; true; true; false; true; true |] -> 9
      | _ -> raise(Exception("Invalid format"))

    let ToSegments (wiring: Wiring) (digit: Digit) : Segments =
      [| for i in 0 .. 6 -> digit.Contains(wiring.[i]) |]

    // who cares about generalization, right?
    let ResolveWiring (digits: Digit []) : Wiring =
      let wiring = [| for i in 0 .. 6 -> '#' |]

      let one = digits |> Seq.find(fun d -> d.Count = 2)
      let seven = digits |> Seq.find(fun d -> d.Count = 3)
      wiring.[0] <- Set.difference(seven) (one) |> Seq.head

      let four = digits |> Seq.find(fun d -> d.Count = 4)
      let fourWith0 = four |> Set.add(wiring.[0])

      let nine =
        digits
        |> Seq.find(fun d -> d.Count = 6 && fourWith0.IsSubsetOf(d))

      wiring.[6] <- Set.difference(nine) (fourWith0) |> Seq.head

      let eight = digits |> Seq.find(fun d -> d.Count = 7)
      wiring.[4] <- Set.difference(eight) (nine) |> Seq.head

      let wires046 =
        Set([ wiring.[0]; wiring.[4]; wiring.[6] ])

      let two =
        digits
        |> Seq.find(fun d -> d.Count = 5 && wires046.IsSubsetOf(d))

      wiring.[2] <- two |> Set.intersect(one) |> Seq.head
      wiring.[5] <- one |> Set.remove(wiring.[2]) |> Seq.head

      wiring.[3] <-
        two
        |> Set.intersect(four)
        |> Set.remove(wiring.[2])
        |> Seq.head

      wiring.[1] <- Set.difference(eight) (Set(wiring)) |> Seq.head

      wiring

    let ReadNumber (digits: seq<int>) : int =
      digits
      |> Seq.map(fun d -> d.ToString())
      |> fun ds -> String.Join("", ds)
      |> Int32.Parse

    let ComputeNumber ((samples, digits): Display) : int =
      let wiring = ResolveWiring(samples)

      digits
      |> Seq.map(ToSegments(wiring))
      |> Seq.map(ReadSegments)
      |> ReadNumber


type Day08(inputText: string) =
  let input = Day08.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day08.FirstChallenge.CountUniquePattern
      |> fun s -> s.ToString()

    member this.SecondChallenge() =
      input
      |> Seq.map(Day08.SecondChallenge.ComputeNumber)
      |> Seq.sum
      |> fun s -> s.ToString()
