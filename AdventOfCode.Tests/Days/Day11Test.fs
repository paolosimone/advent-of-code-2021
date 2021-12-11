module AdventOfCode.Tests.Days.Day11Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day11(inputText)
  Assert.Equal("1656", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day11(inputText)
  Assert.Equal("195", day.SecondChallenge())
