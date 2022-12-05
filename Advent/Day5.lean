import Advent.Util

abbrev Stack := List Char
abbrev Move := Nat × Nat × Nat

section parsing

open Lean Parsec

def parseCrate : Parsec Char := between '[' ']' anyChar

def parseCrate? : Parsec (Option Char) := do
  match (← peek?) with
  | .some '[' => pure $ .some (← parseCrate)
  | .some ' ' => skipString "   " *> pure .none
  | _ => pure .none

def parseCrateLines (width : Nat) : Parsec (Array (Option Char)) := 
  repeat' (parseCrate? <* (skipChar ' ' <|> eof)) width

def getStacks (lines : Array String) : Array Stack := 
  let numStacks := lines[0]! |>.filter (· != ' ') 
                             |>.length
  let parsedLines := lines |>.map (parseCrateLines numStacks).run 
                           |> cleanExcept
  Id.run do 
    let mut init := .mkArray numStacks []
    for line in parsedLines do
      for idx in [:numStacks] do
        match line[idx]! with
        | .some c => init := init.modify idx (.cons c)
        | .none => ()
    return init

def parseMove : Parsec Move := do
  skipString "move "
  let a ← parseNum
  skipString " from "
  let b ← parseNum
  skipString " to "
  let c ← parseNum
  return (a,b -1,c -1)

end parsing

section logic

def applyMove1 (move : Move) (state : Array Stack) : Array Stack :=
  let (num, src, tgt) := move
  match num, state[src]! with
  | 0,  _ 
  | _, [] => state
  | n + 1, top :: rest => let newState := state |>.modify src (fun _ => rest)
                                                |>.modify tgt (.cons top ·)
                          applyMove1 (n, src, tgt) newState

def applyMove2 (move : Move) (state : Array Stack) : Array Stack :=
  let (num, src, tgt) := move
  let chunk := state[src]!.take num
  state |>.modify src (fun st => st.drop num)
        |>.modify tgt (fun st => chunk ++ st)

def applyMoves (strat : Move → Array Stack → Array Stack) (moves : Array Move) (initState : Array Stack) : Array Stack :=
  moves.foldl (fun acc mv => strat mv acc) (init := initState)

end logic

def day5 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 5)
  let splitLines := inputLines.split' (· == "")
  let (crateLines, moveLines) := (splitLines[0]!.reverse, splitLines[1]!)
  let initState := getStacks crateLines
  let moves := moveLines |>.map (parseMove.run) |> cleanExcept
  let answer1 := applyMoves applyMove1 moves initState |>.foldl (fun acc s => s.head! :: acc ) (init := []) 
  let answer2 := applyMoves applyMove2 moves initState |>.foldl (fun acc s => s.head! :: acc ) (init := []) 
  -- dbg_trace moves
  -- dbg_trace answer1
  printAnswer (day := 5) (String.mk answer1.reverse) (String.mk answer2.reverse)

#eval day5
