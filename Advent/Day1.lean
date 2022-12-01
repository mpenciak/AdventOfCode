import Advent.Util

def day1 : IO Unit := do
  let inputLines ← getInputLines false 1
  let parsedInput := inputLines.split' (· == "") |>.map (·.map (fun s => parseNum.run s))
  let cleanedInput := parsedInput.map cleanExcept
  let sortedCalories := cleanedInput.map (Array.sum ·) |>.qsort (· < ·)
  let (top1, top2, top3) := (sortedCalories.back, sortedCalories.pop.back, sortedCalories.pop.pop.back)
  printAnswer 1 top1 (top1 + top2 + top3)

#eval day1
