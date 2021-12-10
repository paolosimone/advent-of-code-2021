module AdventOfCode.Utils
// One of my greatest accomplishments *.*
//
// Usage:
// EarlyReturn() {
//       for number in 1..10 do
//           if number == 3 then
//             return number
//     }
type EarlyReturn() =
  let rec ForUntil (f: 'a -> option<'b>) (list: list<'a>) : option<'b> =
    match list with
    | (head :: tail) ->
      match f(head) with
      | None -> ForUntil(f) (tail)
      | result -> result
    | _ -> None

  member this.For(seq: seq<'a>, f: 'a -> option<'b>) : option<'b> = seq |> Seq.toList |> ForUntil(f)

  member this.Return(x: 'a) : option<'a> = Some(x)

  member this.ReturnFrom(x: option<'a>) : option<'a> = x

  member this.Zero() = None
