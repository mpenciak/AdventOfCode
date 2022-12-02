import Advent.Util

def day1 : IO Unit := do
  let inputLines ← getInputLines (test := false) 1
  let sortedCalories := inputLines |>.split' (· == "") 
                                   |>.map (·.map (fun s => parseNum.run s))
                                   |>.map cleanExcept
                                   |>.map (·.sum)
                                   |>.qsort (· ≥ ·)
  let (top1, top2, top3) := (sortedCalories[0]!, sortedCalories[1]!, sortedCalories[2]!)  
  printAnswer (day := 1) top1 (top1 + top2 + top3)

#eval day1
