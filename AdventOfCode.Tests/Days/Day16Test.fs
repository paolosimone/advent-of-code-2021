module AdventOfCode.Tests.Days.Day16Test

open Xunit
open AdventOfCode.Days

[<Fact>]
let ``FirstChallenge Example 1`` () =
  let day: IDay = Day16("8A004A801A8002F478")
  Assert.Equal("16", day.FirstChallenge())

[<Fact>]
let ``FirstChallenge Example 2`` () =
  let day: IDay = Day16("620080001611562C8802118E34")
  Assert.Equal("12", day.FirstChallenge())

[<Fact>]
let ``FirstChallenge Example 3`` () =
  let day: IDay = Day16("C0015000016115A2E0802F182340")
  Assert.Equal("23", day.FirstChallenge())

[<Fact>]
let ``FirstChallenge Example 4`` () =
  let day: IDay = Day16("A0016C880162017C3686B18A3D4780")
  Assert.Equal("31", day.FirstChallenge())

[<Fact>]
let ``SecondChallenge Example 1`` () =
  let day: IDay = Day16("C200B40A82")
  Assert.Equal("3", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 2`` () =
  let day: IDay = Day16("04005AC33890")
  Assert.Equal("54", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 3`` () =
  let day: IDay = Day16("880086C3E88112")
  Assert.Equal("7", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 4`` () =
  let day: IDay = Day16("CE00C43D881120")
  Assert.Equal("9", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 5`` () =
  let day: IDay = Day16("D8005AC2A8F0")
  Assert.Equal("1", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 6`` () =
  let day: IDay = Day16("F600BC2D8F")
  Assert.Equal("0", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 7`` () =
  let day: IDay = Day16("9C005AC2F8F0")
  Assert.Equal("0", day.SecondChallenge())

[<Fact>]
let ``SecondChallenge Example 8`` () =
  let day: IDay = Day16("9C0141080250320F1802104A08")
  Assert.Equal("1", day.SecondChallenge())
