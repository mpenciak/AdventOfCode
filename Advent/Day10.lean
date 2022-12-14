import Advent.Util

inductive Op
  | addX (n : Int)
  | noop
deriving Repr, BEq

section parser

open Lean Parsec

def parseAddX : Parsec Op := skipString "addx " *> parseInt >>= fun n => pure $ .addX n

def parseNoop : Parsec Op := skipString "noop" *> pure .noop

def parseOp : Parsec Op := parseAddX <|> parseNoop

end parser

section logic

structure CRTState where
  cycle : Nat
  register : Int
  signalStrengths : List Int
  pixel : Nat
  screenPixels : Array Char
deriving Repr

def initCRTState : CRTState := {
  cycle := 1
  register := 1
  signalStrengths := []
  pixel := 0
  screenPixels := #[]
}

abbrev CRTM := StateM CRTState

def incCycle : CRTM Unit := modify fun stt => 
{ stt with 
  cycle := stt.cycle + 1
  pixel := (stt.pixel + 1) % 40 }

def drawPixel : CRTM Unit := do
  let stt ← get
  let sprite := [stt.register - 1, stt.register, stt.register + 1]
  let newPixels := if sprite.contains stt.pixel then stt.screenPixels.push '#' else stt.screenPixels.push '.'
  modify fun stt => {stt with screenPixels := newPixels}

partial def runOp (op : Op) (sndAdd : Bool := false) : CRTM Unit := do
  drawPixel
  let state ← get
  -- dbg_trace reprStr state
  if state.cycle % 40 == 20 then
    modify fun stt => {stt with signalStrengths := (stt.register * stt.cycle) :: state.signalStrengths }
  match op with
  | .noop => incCycle *> return
  | op@(.addX n) => 
    if sndAdd then
      incCycle 
      modify fun stt => {stt with register := stt.register + n}
    else
      incCycle
      runOp op true

def runOps (ops : Array Op) : CRTM Unit := ops.forM runOp

end logic

def day10 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 10)
  let parsedLines := inputLines |>.map parseOp.run
                                |> cleanExcept
  let runOutput := runOps parsedLines |>.run initCRTState 
                                      |>.snd 
  let answer1 := runOutput |>.signalStrengths
                           |>.sum
  let pixels := runOutput.screenPixels
  let mut answer2Array := #[]
  for i in [:6] do
    answer2Array := answer2Array.push (pixels.extract (40*i) (40 * (i + 1)))
  let answer2 : String := answer2Array.foldl (init := "") fun acc row => acc ++ ⟨row.toList⟩ ++ "\n" 
  printAnswer (day := 10) answer1 answer2

#eval day10