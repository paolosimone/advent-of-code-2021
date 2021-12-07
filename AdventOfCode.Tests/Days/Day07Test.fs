module AdventOfCode.Tests.Days.Day07Test

open Xunit
open AdventOfCode.Days

let inputText = @"16,1,2,0,4,2,7,1,2,14"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day07(inputText)
  Assert.Equal("37", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day07(inputText)
  Assert.Equal("168", day.SecondChallenge())
