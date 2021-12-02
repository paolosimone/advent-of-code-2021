module AdventOfCode.Program

open System
open AdventOfCode.Advent
open PrettyTable

let banner =
  @"
    ########  ######  ##     ##    ###    ########  ########
    ##       ##    ## ##     ##   ## ##   ##     ## ##     ##
    ##       ##       ##     ##  ##   ##  ##     ## ##     ##
    ######    ######  ######### ##     ## ########  ########
    ##             ## ##     ## ######### ##   ##   ##
    ##       ##    ## ##     ## ##     ## ##    ##  ##
    ##        ######  ##     ## ##     ## ##     ## ##
       ##    ##    ###    ##     ## #### ########     ###    ########
       ###   ##   ## ##   ##     ##  ##  ##     ##   ## ##   ##     ##
       ####  ##  ##   ##  ##     ##  ##  ##     ##  ##   ##  ##     ##
       ## ## ## ##     ## ##     ##  ##  ##     ## ##     ## ##     ##
       ##  #### #########  ##   ##   ##  ##     ## ######### ##     ##
       ##   ### ##     ##   ## ##    ##  ##     ## ##     ## ##     ##
       ##    ## ##     ##    ###    #### ########  ##     ## ########

"

let inputFolder = "Input"

module Report =
  let private header =
    [ "Day"
      "LoadElapsed"
      "FirstResult"
      "FirstElapsed"
      "SecondResult"
      "SecondElapsed" ]

  let private formatDuration (duration: TimeSpan) : string =
    String.Format("{0:F6} s", duration.TotalSeconds)

  let private ToRow (dayResult: DayResult) =
    [ dayResult.Day.ToString("D2")
      dayResult.LoadElapsed |> formatDuration
      dayResult.FirstResult
      dayResult.FirstElapsed |> formatDuration
      dayResult.SecondResult
      dayResult.SecondElapsed |> formatDuration ]

  let Print (dayResults: DayResult []) =
    dayResults
    |> Seq.map(ToRow)
    |> Seq.toList
    |> prettyTable
    |> withHeaders(header)
    |> printTable

[<EntryPoint>]
let Main args =
  let results =
    match args with
    | [| number |] -> [| RunDay(inputFolder) (Int32.Parse(number)) |]
    | _ -> RunAllDays(inputFolder)

  Console.WriteLine(banner)
  Report.Print(results)
  0
