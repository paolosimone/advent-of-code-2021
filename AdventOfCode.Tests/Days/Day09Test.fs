module AdventOfCode.Tests.Days.Day09Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"2199943210
3987894921
9856789892
8767896789
9899965678"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day09(inputText)
  Assert.Equal("15", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day09(inputText)
  Assert.Equal("1134", day.SecondChallenge())
