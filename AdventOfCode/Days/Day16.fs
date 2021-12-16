namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day16 =
  type Literal =
    { Version: int
      BitLength: int
      Value: int64 }

  and OperatorLength =
    | Bits of int
    | Packets of int

  and PacketType =
    | Number
    | Sum
    | Product
    | Min
    | Max
    | Greater
    | Less
    | Equal

  and Operator =
    { Version: int
      Type: PacketType
      Length: OperatorLength
      Operands: List<Expression> }

  and Expression =
    | Literal of Literal
    | Operator of Operator

  let HexToBinary (input: string) : string =
    input
    |> Seq.map
         (fun digit ->
           Convert
             .ToString(Convert.ToInt32(digit.ToString(), 16), 2)
             .PadLeft(4, '0'))
    |> fun binaries -> String.Join("", binaries)

  let BinaryToDecimal (binary: string) : int = Convert.ToInt32(binary, 2)

  let Type (bits: string, start: int) : PacketType =
    let offset (i) = start + i

    match bits.[offset(3)..offset(5)] with
    | "000" -> Sum
    | "001" -> Product
    | "010" -> Min
    | "011" -> Max
    | "100" -> Number
    | "101" -> Greater
    | "110" -> Less
    | "111" -> Equal
    | _ -> raise(Exception("invalid type"))

  let ParseLiteral (bits: string, start: int) : (Literal * int) =
    let offset (i) = start + i

    let digits = List<string>()
    let mutable hasNext = true

    while hasNext do
      let digitStart = offset(6) + 5 * digits.Count
      digits.Add(bits.[(digitStart + 1)..(digitStart + 4)])
      hasNext <- bits.[digitStart] = '1'

    let length = 6 + digits.Count * 5

    let literal =
      { Version = BinaryToDecimal(bits.[offset(0)..offset(2)])
        BitLength = length
        Value = Convert.ToInt64(String.Join("", digits), 2) }

    (literal, offset(length))

  let ParseOperator (bits: string, start: int) : (Operator * int) =
    let offset (i) = start + i

    let version =
      BinaryToDecimal(bits.[offset(0)..offset(2)])

    let (length, next) =
      match bits.[offset(6)] with
      | '0' ->
        let nextStart =
          offset(22)
          + BinaryToDecimal(bits.[offset(7)..offset(21)])

        (Bits(nextStart), offset(22))
      | _ ->
        let count =
          BinaryToDecimal(bits.[offset(7)..offset(17)])

        (Packets(count), offset(18))

    let operator =
      { Version = version
        Type = Type(bits, start)
        Length = length
        Operands = List<Expression>() }

    (operator, next)

  let IsClosed (operator: Operator, index: int) : bool =
    match operator.Length with
    | Bits (next) when next = index -> true
    | Packets (count) when count = operator.Operands.Count -> true
    | _ -> false

  let ConsumeToken (bits: string, start: int) : (Expression * int) =
    match Type(bits, start) with
    | Number ->
      let (token, next) = ParseLiteral(bits, start)
      (Literal(token), next)

    | _ ->
      let (token, next) = ParseOperator(bits, start)
      (Operator(token), next)

  let Parse (hex: string) : Expression =
    let bits = HexToBinary(hex)
    let mutable (expr, i) = ConsumeToken(bits, 0)
    let stack = Stack<Expression>([ expr ])

    let Collapse () =
      let expr = stack.Pop()

      match stack.Peek() with
      | Operator (op) -> op.Operands.Add(expr)
      | _ -> raise(Exception("impossible"))

    let Completed (i) =
      stack.Count = 1
      && match stack.Peek() with
         | Literal (_) -> true
         | Operator (op) when IsClosed(op, i) -> true
         | _ -> false

    while not(Completed(i)) do
      match stack.Peek() with
      | Literal (_) -> Collapse()

      | Operator (op) when IsClosed(op, i) -> Collapse()

      | _ ->
        let (expr, next) = ConsumeToken(bits, i)
        stack.Push(expr)
        i <- next

    stack.Pop()

  module FirstChallenge =
    let rec SumVersions (expr: Expression) : int =
      match expr with
      | Literal (literal) -> literal.Version
      | Operator (op) ->
        op.Version
        + (op.Operands |> Seq.map(SumVersions) |> Seq.sum)

  module SecondChallenge =
    let rec Resolve (expr: Expression) : int64 =
      match expr with
      | Literal (literal) -> literal.Value
      | Operator (op) ->
        let operands =
          op.Operands |> Seq.map(Resolve) |> Seq.toArray

        match op.Type with
        | Sum -> operands |> Seq.sum
        | Product -> operands |> Seq.reduce((*))
        | Min -> operands |> Seq.min
        | Max -> operands |> Seq.max
        | Greater -> Convert.ToInt64(operands.[0] > operands.[1])
        | Less -> Convert.ToInt64(operands.[0] < operands.[1])
        | Equal -> Convert.ToInt64(operands.[0] = operands.[1])
        | _ -> raise(Exception("unknown operand"))

type Day16(inputText: string) =
  let input = Day16.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      input
      |> Day16.FirstChallenge.SumVersions
      |> fun res -> res.ToString()

    member this.SecondChallenge() =
      input
      |> Day16.SecondChallenge.Resolve
      |> fun res -> res.ToString()
