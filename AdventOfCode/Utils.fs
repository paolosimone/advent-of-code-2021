module AdventOfCode.Utils
// One of my greatest accomplishments *.*
//
// Functional purists look away, for your own safety, really
//
// Usage:
//
// EarlyReturn() {
//   for number in 1..10 do
//     if number == 3 then
//       return number
// }
type EarlyReturn() =
  member this.For(seq: seq<'a>, f: 'a -> option<'b>) : option<'b> =
    let mutable result: option<'b> = None

    seq
    |> Seq.skipWhile
         (fun a ->
           result <- f(a)
           result.IsNone)
    |> Seq.tryHead
    |> ignore

    result

  member this.While(keepGoing: unit -> bool, step: unit -> option<'a>) : option<'a> =
    let mutable result: option<'a> = None

    while keepGoing() && result.IsNone do
      result <- step()

    result

  member this.Delay(f: unit -> option<'a>) = f

  member this.Run(f: unit -> option<'a>) = f()

  member this.Return(x: 'a) : option<'a> = Some(x)

  member this.Zero() = None
