namespace AdventOfCode.Days

open System
open System.Collections.Generic

module private Day04 =
  type Number = int
  type Board = Number [] []

  type Input =
    { Numbers: Number []
      Boards: Board [] }

  let ParseNumbers (line: string) : Number [] =
    line
    |> fun s -> s.Split(",")
    |> Array.map(Int32.Parse)

  let ParseBoard (lines: string) : Board =
    lines
    |> fun s -> s.Split(Environment.NewLine)
    |> Array.map
         (fun line ->
           line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
           |> Array.map(Int32.Parse))

  let Parse (inputText: string) : Input =
    let blocks =
      inputText.Split(Environment.NewLine + Environment.NewLine)

    { Numbers = ParseNumbers(blocks.[0])
      Boards = blocks.[1..] |> Array.map(ParseBoard) }

  type BoardIndex = int
  type RowIndex = int
  type ColumnIndex = int
  type NumberCoords = RowIndex * ColumnIndex
  type InvertedIndex = Dictionary<Number, List<BoardIndex * NumberCoords>>

  let BuildInvertedIndex (boards: Board []) : InvertedIndex =
    let invertedIndex = new InvertedIndex()

    for boardIndex = 0 to boards.Length - 1 do
      let board = boards.[boardIndex]

      for rowIndex = 0 to board.Length - 1 do
        let row = board.[rowIndex]

        for colIndex = 0 to row.Length - 1 do
          let number = row.[colIndex]

          if not(invertedIndex.ContainsKey(number)) then
            invertedIndex.Add(number, new List<BoardIndex * NumberCoords>())

          invertedIndex.[number]
            .Add((boardIndex, (rowIndex, colIndex)))

    invertedIndex

  type BoardMarker(board: Board) =
    let rowSize = board.Length
    let rows = [| for _ in 1 .. rowSize -> 0 |]

    let colSize = board.[0].Length
    let columns = [| for _ in 1 .. colSize -> 0 |]

    member this.MarkAndCheck((rowIndex, colIndex): NumberCoords) : bool =
      rows.[colIndex] <- rows.[colIndex] + 1
      columns.[rowIndex] <- columns.[rowIndex] + 1

      rows.[colIndex] = rowSize
      || columns.[rowIndex] = colSize

    override this.ToString() : string = sprintf "%A" (rows, columns)

  type Winner = Number * BoardIndex

  let FindWinners (input: Input) : Winner [] =
    let invertedIndex = BuildInvertedIndex(input.Boards)

    let counters = Array.map(BoardMarker) (input.Boards)

    let mutable winners = List<Winner>()

    let mutable alreadyWon =
      [| for _ in 1 .. (input.Boards.Length) -> false |]

    for number in input.Numbers do
      for (boardIndex, coords) in invertedIndex.[number] do
        if
          not(alreadyWon.[boardIndex])
          && counters.[boardIndex].MarkAndCheck(coords)
        then
          winners.Add((number, boardIndex))
          alreadyWon.[boardIndex] <- true

    winners.ToArray()

  let ComputeScore (input: Input, (number, boardIndex): Winner) : int =
    let round = Array.IndexOf(input.Numbers, number)
    let marked = Set(input.Numbers.[..round])

    let unmarkedSum =
      input.Boards.[boardIndex]
      |> Seq.collect(fun n -> n)
      |> Seq.filter(fun n -> not(marked.Contains(n)))
      |> Seq.sum

    unmarkedSum * number

type Day04(inputText: string) =
  let input = Day04.Parse(inputText)

  interface IDay with
    member this.FirstChallenge() =
      let winner = Day04.FindWinners(input).[0]
      let score = Day04.ComputeScore(input, winner)
      score.ToString()

    member this.SecondChallenge() =
      let winner = Day04.FindWinners(input) |> Array.last
      let score = Day04.ComputeScore(input, winner)
      score.ToString()
