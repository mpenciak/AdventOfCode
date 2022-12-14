import Advent.Util
import Lean.Data.HashSet

inductive SqMove
  | U (n : Nat)
  | R (n : Nat)
  | D (n : Nat)
  | L (n : Nat)
deriving Repr

inductive Dir | U | R | D | L
deriving Repr

def SqMove.toDir : SqMove → Dir
  | .U _ => .U
  | .R _ => .R
  | .D _ => .D
  | .L _ => .L

section parsing

open Lean Parsec

def pU : Parsec SqMove := skipString "U " *> parseNum >>= fun n => pure $ .U n

def pR : Parsec SqMove := skipString "R " *> parseNum >>= fun n => pure $ .R n

def pD : Parsec SqMove := skipString "D " *> parseNum >>= fun n => pure $ .D n

def pL : Parsec SqMove := skipString "L " *> parseNum >>= fun n => pure $ .L n

def parseSqMove : Parsec SqMove := pU <|> pR <|> pD <|> pL

end parsing

section logic

abbrev IntPos := Int × Int

instance : Add IntPos where
  add := fun (a, b) (c, d) => (a + c, b + d)

instance : Neg IntPos where
  neg := fun (a, b) => (-a, -b)

instance : Sub IntPos where
  sub x y := x + -y

def IntPos.scale : IntPos → IntPos
  | (x, y) => (x/x.natAbs, y/y.natAbs)

def distance : IntPos → IntPos → Nat
  | (x₁, y₁), (x₂, y₂) => max (x₁ - x₂).natAbs (y₁ - y₂).natAbs

structure Rope where
  head : IntPos
  tail : IntPos
deriving Repr

def Rope.moveTail (rope : Rope) : Rope :=
  if distance rope.head rope.tail > 1 then 
    let disp := rope.head - rope.tail |>.scale
    {rope with tail := rope.tail + disp}
  else rope

def Rope.moveDir (rope : Rope) : Dir → Rope
  | .U => {rope with head := rope.head + (0, 1)}.moveTail
  | .R => {rope with head := rope.head + (1, 0)}.moveTail
  | .D => {rope with head := rope.head + (0, -1)}.moveTail
  | .L => {rope with head := rope.head + (-1, 0)}.moveTail

open Lean (HashSet)

structure State where 
  ropeState : Rope
  tailHist : HashSet IntPos

def initRope : State := 
  { ropeState := ⟨(0,0), (0,0)⟩
    tailHist := .empty }

abbrev MoveM := StateM State

def applyRopeMove (move : SqMove) : MoveM Unit := do
  match move with
  | .U (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .U
      ⟨newRope, hist.insert newRope.tail⟩
    applyRopeMove (.U n)
  | .R (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .R
      ⟨newRope, hist.insert newRope.tail⟩
    applyRopeMove (.R n)
  | .D (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .D
      ⟨newRope, hist.insert newRope.tail⟩
    applyRopeMove (.D n)
  | .L (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .L
      ⟨newRope, hist.insert newRope.tail⟩
    applyRopeMove (.L n)
  | _ => pure ()

def applyRopeMoves (hist : Array SqMove) : MoveM Unit := hist.forM fun move => applyRopeMove move

abbrev LongRope := Array IntPos

def initLongRope : LongRope := .mkArray 10 (0,0)

def LongRope.moveTails (rope : LongRope) : LongRope := Id.run do
  let mut answer := #[rope[0]!]
  for idx in [1:rope.size] do
    let newTailPos := if distance rope[idx - 1]! rope[idx]! > 1 then 
      let disp := rope[idx -1]! - rope[idx]! |>.scale
      rope[idx]! + disp
    else rope[idx]!
    answer := answer.push newTailPos
  return answer

def LongRope.moveDir (rope : LongRope) : Dir → LongRope
  | .U => .moveTails $ rope.set! 0 (rope[0]! + (0,1))
  | .R => .moveTails $ rope.set! 0 (rope[0]! + (1,0))
  | .D => .moveTails $ rope.set! 0 (rope[0]! + (0,-1))
  | .L => .moveTails $ rope.set! 0 (rope[0]! + (-1,0))

open Lean (HashSet)

structure LongRopeState where 
  ropeState : LongRope
  tailHist : HashSet IntPos

def initLongRopeState : LongRopeState := 
  { ropeState := initLongRope
    tailHist := .empty }

abbrev LongMoveM := StateM LongRopeState

def applyLongMove (move : SqMove) : LongMoveM Unit := do
  match move with
  | .U (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .U
      ⟨newRope, hist.insert newRope.back⟩
    applyLongMove (.U n)
  | .R (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .R
      ⟨newRope, hist.insert newRope.back⟩
    applyLongMove (.R n)
  | .D (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .D
      ⟨newRope, hist.insert newRope.back⟩
    applyLongMove (.D n)
  | .L (n + 1) => 
    modify fun ⟨rope, hist⟩ => 
      let newRope := rope.moveDir .L
      ⟨newRope, hist.insert newRope.back⟩
    applyLongMove (.L n)
  | _ => pure ()

def applyLongMoves (hist : Array SqMove) : LongMoveM Unit := hist.forM fun move => applyLongMove move

end logic

def day9 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 9)
  let parsedLines := inputLines |>.map parseSqMove.run
                                |> cleanExcept
  let answer1 := applyRopeMoves parsedLines |>.run initRope
                                        |>.snd
                                        |>.tailHist
                                        |>.toList
                                        |>.length
  let answer2 := applyLongMoves parsedLines |>.run initLongRopeState
                                            |>.snd
                                            |>.tailHist
                                            |>.toList
                                            |>.length
  printAnswer (day := 9) answer1 answer2

#eval day9