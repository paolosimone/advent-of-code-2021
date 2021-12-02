module AdventOfCode.Tests.Days.Day01Test

open Xunit
open AdventOfCode.Days

[<Fact>]
let ``First Challenge`` () =
  let inputText =
    @"199
200
208
210
200
207
240
269
260
263"

  let day: IDay = Day01(inputText)
  Assert.Equal("7", day.FirstChallenge())

[<Fact>]
let ``Second Challenge`` () =
  let inputText =
    @"199
200
208
210
200
207
240
269
260
263"

  let day: IDay = Day01(inputText)
  Assert.Equal("5", day.SecondChallenge())
