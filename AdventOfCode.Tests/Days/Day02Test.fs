module AdventOfCode.Tests.Days.Day02Test

open Xunit
open AdventOfCode.Days

[<Fact>]
let ``FirstChallenge`` () =
  let inputText =
    @"forward 5
down 5
forward 8
up 3
down 8
forward 2"

  let day: IDay = Day02(inputText)
  Assert.Equal("150", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let inputText =
    @"forward 5
down 5
forward 8
up 3
down 8
forward 2"

  let day: IDay = Day02(inputText)
  Assert.Equal("900", day.SecondChallenge())
