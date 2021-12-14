namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day14 =
  type Element = char
  type Pair = Element * Element
  type Template = string

  type Formula = Dictionary<Pair, int64>
  type Rules = Dictionary<Pair, Element>

  type Input = { Template: Template; Rules: Rules }

  let ParseInsertion (line: string) : KeyValuePair<Pair, Element> =
    line
    |> fun s -> s.Split(" -> ")
    |> function
      | [| pair; elem |] -> KeyValuePair((pair.[0], pair.[1]), elem.[0])
      | _ -> raise(Exception("invalid input"))

  let Parse (inputText: string) : Input =
    let (template, rules) =
      inputText
      |> fun s -> s.Split(Environment.NewLine + Environment.NewLine)
      |> function
        | [| template; rules |] -> (template, rules.Split(Environment.NewLine))
        | _ -> raise(Exception("invalid input"))

    { Template = template
      Rules = rules |> Array.map(ParseInsertion) |> Rules }

  let Insert ((a, b): Pair, elem: Element) : Pair * Pair = ((a, elem), (elem, b))

  let Step (rules: Rules) (formula: Formula) : Formula =
    let next = Formula()

    for pairCount in formula do
      let pair = pairCount.Key

      if rules.ContainsKey(pair) then
        let (left, right) = Insert(pair, rules.[pair])
        next.[left] <- next.GetValueOrDefault(left) + pairCount.Value
        next.[right] <- next.GetValueOrDefault(right) + pairCount.Value
      else
        next.[pair] <- next.GetValueOrDefault(pair) + pairCount.Value

    next

  let InitFormula (template: Template) : Formula =
    (template + "#")
    |> Seq.windowed(2)
    |> Seq.map
         (function
         | [| left; right |] -> (left, right)
         | _ -> raise(Exception("unreachable")))
    |> Seq.countBy(id)
    |> Seq.map(fun (pair, count) -> KeyValuePair(pair, int64 count))
    |> Formula

  let DeltaCount (formula: Formula) : int64 =
    let counter = Dictionary<Element, int64>()

    for pairCount in formula do
      let (left, _) = pairCount.Key
      counter.[left] <- counter.GetValueOrDefault(left) + pairCount.Value

    Seq.max(counter.Values) - Seq.min(counter.Values)

  let Run (steps: int) (input: Input) : int64 =
    let applyStep =
      fun formula _ -> Step(input.Rules) (formula)

    let formula = InitFormula(input.Template)

    [ 1 .. steps ]
    |> Seq.fold(applyStep) (formula)
    |> DeltaCount


type Day14(inputText: string) =
  let input = Day14.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day14.Run(10)
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      input
      |> Day14.Run(40)
      |> fun res -> res.ToString()
