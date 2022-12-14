import Advent.Util
import Lean.Data.HashSet

abbrev Pos := Nat × Nat

abbrev Grid := Array (Array Nat)

section parsing

open Lean Parsec

def parseLine : Parsec $ Array Nat := many parseDigit

end parsing

section logic

open Lean (HashSet)

def column (grid : Grid) (c : Nat) : Array Nat := Id.run do
  let mut answer := #[]
  for row in grid do
    answer := answer.push row[c]!
  return answer

def transpose (grid : Grid) : Grid := Id.run do
  let cols := grid[0]!.size
  let mut answer := #[]
  for idx in [:cols] do
    answer := answer.push (column grid idx)
  return answer

def getVisible (grid : Grid) : HashSet Pos := Id.run do
  let mut answer := .empty
  let numRows := grid.size - 1
  let numCols := grid[0]!.size - 1
  for r in [:numRows] do
    let row := grid[r]!
    let mut localMax : Int := -1
    for c in [:numCols] do
      if row[c]! > localMax then
        localMax := row[c]!
        answer := answer.insert (r, c)
    localMax := -1
    for c in [:numCols] do
      if row[numCols - c]! > localMax then
        localMax := row[numCols - c]!
        answer := answer.insert (r, numCols - c)
  let tgrid := transpose grid
  for c in [:numCols] do
    let col := tgrid[c]!
    let mut localMax : Int := -1
    for r in [:numRows] do
      if col[r]! > localMax then
        localMax := col[r]!
        answer := answer.insert (r, c)
    localMax := -1
    for r in [:numRows] do
      if col[numRows - r]! > localMax then
        localMax := col[numRows - r]!
        answer := answer.insert (numRows - r, c)
  return answer

def allTrees (numRows numCols : Nat) : HashSet Pos := Id.run do
  let mut answer := .empty
  for r in [1:numRows-1] do
    for c in [1:numCols-1] do
      answer := answer.insert (r,c)
  return answer

def getScenicScore (pos : Pos) (grid : Grid) : Nat := Id.run do
  let tgrid := transpose grid
  let posHeight := grid[pos.fst]![pos.snd]!
  let row := grid[pos.fst]!
  let col := tgrid[pos.snd]!
  let mut rscore := 0
  for idx in [pos.snd + 1 : row.size] do
    rscore := rscore + 1
    if row[idx]! ≥ posHeight then
      break
  let mut lscore := 0
  for idx in [1:pos.snd + 1] do
    lscore := lscore + 1
    if row[pos.snd - idx]! ≥ posHeight then
      break
  let mut dscore := 0
  for idx in [pos.fst + 1 : col.size] do
    dscore := dscore + 1
    if col[idx]! ≥ posHeight then
      break
  let mut uscore := 0
  for idx in [1:pos.fst + 1] do
    uscore := uscore + 1
    if col[pos.fst - idx]! ≥ posHeight then
      break
  return rscore * lscore * dscore * uscore

end logic

def day8 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 8)
  let parsedInput := inputLines |>.map parseLine.run
                                |> cleanExcept
  let visible := getVisible parsedInput |>.insert (parsedInput.size - 1, parsedInput[0]!.size -1)
  let answer1 := visible.toArray.size

  let all := allTrees parsedInput.size parsedInput.size 
  let mut scenicScores := []
  for pos in all do
    scenicScores := scenicScores.cons (getScenicScore pos parsedInput)
  let answer2? := scenicScores.maximum?
  let answer2 := match answer2? with
                 | .some n => n
                 | .none => 0 
  printAnswer (day := 8) answer1 answer2

#eval day8