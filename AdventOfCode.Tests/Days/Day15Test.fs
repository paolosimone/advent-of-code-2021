module AdventOfCode.Tests.Days.Day15Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day15(inputText)
  Assert.Equal("40", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day15(inputText)
  Assert.Equal("315", day.SecondChallenge())
