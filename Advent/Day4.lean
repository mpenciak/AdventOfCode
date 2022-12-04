import Advent.Util

abbrev Range := Nat × Nat

def Range.contains : Range → Range → Bool
  | (l₁, u₁), (l₂, u₂) => l₁ ≤ l₂ && u₁ ≥ u₂

def Range.overlaps : Range → Range → Bool
  | (l₁, u₁), (l₂, u₂) => (l₁ ≤ l₂ && l₂ ≤ u₁) || (l₂ ≤ l₁ && l₁ ≤ u₂)

def day4 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 4)
  let parsedInputs : Array (Range × Range) := 
    inputLines |>.map (fun s => s.split' (· == ','))
               |>.map (fun (s₁, s₂) => (s₁.split' (· == '-'), s₂.split' (· == '-')))
               |>.map (fun ((l₁,u₁), (l₂,u₂)) => ((l₁.toNat!, u₁.toNat!), (l₂.toNat!, u₂.toNat!)))
  let answer1 := parsedInputs |>.map (fun (r₁, r₂) => r₁.contains r₂ || r₂.contains r₁)
                              |>.map (fun b => if b then 1 else 0)
                              |>.sum
  let answer2 := parsedInputs |>.map (fun (r₁, r₂) => r₁.overlaps r₂)
                              |>.map (fun b => if b then 1 else 0)
                              |>.sum
  printAnswer (day := 4) answer1 answer2

#eval day4