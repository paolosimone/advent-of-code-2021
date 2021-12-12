module AdventOfCode.Tests.Days.Day12Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day12(inputText)
  Assert.Equal("226", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day12(inputText)
  Assert.Equal("3509", day.SecondChallenge())
