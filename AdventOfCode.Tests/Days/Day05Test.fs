module AdventOfCode.Tests.Days.Day05Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day05(inputText)
  Assert.Equal("5", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day05(inputText)
  Assert.Equal("12", day.SecondChallenge())
