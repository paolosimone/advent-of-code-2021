module AdventOfCode.Advent

open System
open System.Diagnostics
open System.IO
open AdventOfCode.Days

let private Timed (task) =
  let clock = Stopwatch()
  clock.Start()
  let result = task()
  (result, clock.Elapsed)

let private ReadDayInput (inputFolder: string, number: int) : string =
  let dayInput = String.Format("Day{0:D2}", number)
  let path = Path.Combine(inputFolder, dayInput)
  File.ReadAllText(path)

type private DayLoader = string -> IDay

let private days: DayLoader [] =
  [| fun input -> Day01(input)
     fun input -> Day02(input)
     fun input -> Day03(input)
     fun input -> Day04(input)
     fun input -> Day05(input)
     fun input -> Day06(input)
     fun input -> Day07(input)
     fun input -> Day08(input)
     fun input -> Day09(input)
     fun input -> Day10(input)
     fun input -> Day11(input)
     fun input -> Day12(input)
     fun input -> Day13(input)
     fun input -> Day14(input)
     fun input -> Day15(input) |]

type DayResult =
  { Day: int
    LoadElapsed: TimeSpan
    FirstResult: string
    FirstElapsed: TimeSpan
    SecondResult: string
    SecondElapsed: TimeSpan }

let RunDay (inputFolder: string) (number: int) : DayResult =
  let input = ReadDayInput(inputFolder, number)
  let loadDay = days.[number - 1]
  let (day, loadElapsed) = Timed(fun () -> loadDay(input))
  let (firstResult, firstElapsed) = Timed(fun () -> day.FirstChallenge())
  let (secondResult, secondElapsed) = Timed(fun () -> day.SecondChallenge())

  { Day = number
    LoadElapsed = loadElapsed
    FirstResult = firstResult
    FirstElapsed = firstElapsed
    SecondResult = secondResult
    SecondElapsed = secondElapsed }

let RunAllDays (inputFolder: string) : DayResult [] =
  [| 1 .. days.Length |]
  |> Array.map(RunDay(inputFolder))
