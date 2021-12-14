module AdventOfCode.Tests.Days.Day14Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day14(inputText)
  Assert.Equal("1588", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day14(inputText)
  Assert.Equal("2188189693529", day.SecondChallenge())
