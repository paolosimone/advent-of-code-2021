module AdventOfCode.Tests.Days.Day06Test

open Xunit
open AdventOfCode.Days

let inputText = @"3,4,3,1,2"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day06(inputText)
  Assert.Equal("5934", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day06(inputText)
  Assert.Equal("26984457539", day.SecondChallenge())
