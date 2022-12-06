import Advent.Util
import Std.Data.List.Basic

def init (sit : String.Iterator) (len : Nat) : List Char :=
  let rec go (sit : String.Iterator) (acc : List Char) : Nat → List Char
    | 0       => acc
    | .succ n => go (sit.next) (acc.cons sit.curr) n
  go sit [] len

def allUniques (sit : String.Iterator) (cs : List Char) : String.Iterator :=
  if sit.atEnd then sit else
    let cs' := cs.rotateRight.drop 1 |>.cons sit.curr
    if cs'.Nodup then sit else
      allUniques sit.next cs'

def day6 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 6)
  let testLine := inputLines[0]!.mkIterator
  let sit1 := allUniques (testLine.nextn 4) (init testLine 4)
  let answer1 := sit1.pos.byteIdx + 1
  let sit2 := allUniques (testLine.nextn 14) (init testLine 14)
  let answer2 := sit2.pos.byteIdx + 1
  printAnswer (day := 6) answer1 answer2

#eval day6