module AdventOfCode.Tests.Days.Day10Test

open Xunit
open AdventOfCode.Days

let inputText =
  @"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

[<Fact>]
let ``FirstChallenge`` () =
  let day: IDay = Day10(inputText)
  Assert.Equal("26397", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge`` () =
  let day: IDay = Day10(inputText)
  Assert.Equal("288957", day.SecondChallenge())
