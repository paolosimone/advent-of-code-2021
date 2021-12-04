namespace AdventOfCode.Days

open System

module private Day03 =
  type Input = string []

  let Parse (inputText: string) : Input =
    inputText
    |> fun s -> s.Split(Environment.NewLine)
    |> Seq.toArray

  module FirstChallenge =
    let BinaryOpposite (digits: int) (number: int) : int = (1 <<< digits) - 1 - number

    // Naive, but hey, it does its job
    let MostCommon (input: Input) : int =
      let n = input.[0].Length

      // reduce by column
      let acc = [| for _ in 1 .. n -> 0 |]

      for row in input do
        for i = 0 to acc.Length - 1 do
          let delta = if row.[i] = '1' then 1 else -1
          acc.[i] <- acc.[i] + delta

      // convert back to binary
      acc
      |> Array.map
           (function
           | x when x > 0 -> "1"
           | _ -> "0")
      |> fun array -> String.Join("", array)
      |> fun binary -> Convert.ToInt32(binary, 2)


  module SecondChallenge =
    let rec private BinarySearchFirstOne (input: Input, digit: int, (start, stop): int * int) : option<int> =
      let middle = (start + stop) / 2

      match (input.[start].[digit], input.[middle].[digit], input.[stop].[digit]) with
      | ('0', '0', '1') -> BinarySearchFirstOne(input, digit, (middle + 1, stop))
      | ('0', '1', '1') -> BinarySearchFirstOne(input, digit, (start + 1, middle))
      | ('1', '1', '1') -> Some(start)
      | _ -> None

    let rec private FindRecursive
      (
        input: Input,
        keepZero: (int * int) -> bool,
        digit: int,
        (start, stop): (int * int)
      ) : int =
      if start = stop then
        Convert.ToInt32(input.[start], 2)
      else
        let remaining = stop - start + 1

        let zeroCount =
          match BinarySearchFirstOne(input, digit, (start, stop)) with
          | Some (firstOne) -> firstOne - start
          | None -> remaining

        let nextSlice =
          if keepZero(zeroCount, remaining) then
            (start, start + zeroCount - 1)
          else
            (start + zeroCount, stop)

        FindRecursive(input, keepZero, digit + 1, nextSlice)

    let FindOxigen (input: Input) : int =
      let keepZero =
        fun (zeroCount, remaining) -> zeroCount > remaining / 2

      FindRecursive(input, keepZero, 0, (0, input.Length - 1))

    let FindCO2 (input: Input) : int =
      let keepZero =
        fun (zeroCount, remaining) -> zeroCount <= remaining / 2

      FindRecursive(input, keepZero, 0, (0, input.Length - 1))


type Day03(inputText: string) =
  let input = Day03.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      let mostCommon = Day03.FirstChallenge.MostCommon(input)

      let leastCommon =
        Day03.FirstChallenge.BinaryOpposite(input.[0].Length) (mostCommon)

      (mostCommon * leastCommon).ToString()

    member this.SecondChallenge() =
      let sorted = Array.sort(input)
      let oxigen = Day03.SecondChallenge.FindOxigen(sorted)
      let co2 = Day03.SecondChallenge.FindCO2(sorted)
      (oxigen * co2).ToString()
