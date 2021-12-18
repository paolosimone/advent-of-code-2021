module AdventOfCode.Tests.Days.Day17Test

open Xunit
open AdventOfCode.Days

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day17("x=20..30|y=-10..-5")
  Assert.Equal("45", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day17("x=20..30|y=-10..-5")
  Assert.Equal("112", day.SecondChallenge())
