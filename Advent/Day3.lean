import Advent.Util
import Std.Data.List.Basic

def itemPriority (c : Char) : Nat := 
  if c ≥ 'a' then c.toNat - 96 else c.toNat - 38

def extractTriples [Inhabited α] (arr : Array α) : Array (α × α × α) := Id.run do
  let mut ans := #[]
  for n in [:arr.size / 3] do
    let temp := arr.extract (3 * n) (3 * n + 3)
    ans := ans.push (temp[0]!, temp[1]!, temp[2]!)
  return ans

def day3 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 3)

  let compartments := inputLines.map fun s => s.data.splitAt (s.length / 2)
  let commonItems := compartments |>.map fun (s₁, s₂) => s₁.inter s₂
                                  |>.head!
  let answer1 := commonItems |>.map itemPriority
                             |>.sum
  
  let groupBadges := extractTriples inputLines |>.map 
    fun (s₁, s₂, s₃) => (s₁.data.inter s₂.data).inter s₃.data

  let answer2 := groupBadges |>.map List.head!
                             |>.map itemPriority
                             |>.sum
  printAnswer (day := 3) answer1 answer2

#eval day3


