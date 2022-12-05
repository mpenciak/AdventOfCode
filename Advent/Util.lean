import Lean.Data.Parsec

section IO_utils

open System FilePath in
def inputPath (test : Bool) (day : Nat) : FilePath := 
  let parentDir : FilePath := if test then "Test" else "Input"
  parentDir / s!"input{day}.txt" 

def getInputLines (test : Bool) (day : Nat) : IO (Array String) := IO.FS.lines $ inputPath test day

open IO in
def printAnswer [ToString α] (day : Nat) (ans₁ ans₂ : α) : IO Unit :=
  println s!"Day {day} Answers:" *>
  println "==================" *>
  println "Solution 1" *> 
  println "----------" *>
  println ans₁ *> 
  println "Solution 2" *> 
  println "----------" *> 
  println ans₂

end IO_utils

section data_utils

def Array.split' (p : α → Bool) (a : Array α) : Array (Array α) := Id.run do
  let mut ans := #[]
  let mut ans' := #[]
  for x in a do
    if p x then 
      ans := ans.push ans'
      ans':= #[] else
    ans':= ans'.push x
  ans := ans.push ans'
  return ans

def String.split' (s : String) (p : Char → Bool) : String × String := 
  let s₁ := s.takeWhile (not ∘ p)
  let s₂ := s.takeRight (s.length - s₁.length - 1)
  (s₁, s₂)

def Char.toDigit (c : Char) : Nat := c.toNat - 48

def List.sum {α : Type _} [Add α] [OfNat α (nat_lit 0)] (ns : List α) : α := 
  ns.foldl (init := 0) fun acc n => acc + n

def Array.sum {α : Type _} [Add α] [OfNat α (nat_lit 0)] (ns : Array α) : α := 
  ns.foldl (init := 0) fun acc n => acc + n

def cleanExcept {ε α : Type _} (ear : Array (Except ε α)) : Array α := 
  ear.foldl (init := #[]) fun as ea => 
    match ea with
    | .ok a    => as.push a
    | .error _ => as

def String.filter (p : Char → Bool) (s : String) : String := ⟨s.data.filter p⟩

end data_utils

section parser_utils 

namespace Lean.Parsec

def parseDigit : Parsec Nat := attempt do
  let n ← digit
  return n.toDigit

def parseNum : Parsec Nat := 
  many1 parseDigit >>= fun ns => pure $ ns.foldl (fun n m => 10 * n + m) 0

def between (b e : Char) (p : Parsec α) : Parsec α := 
  skipChar b *> p >>= fun c => skipChar e *> pure c

@[specialize]
def repeatCore (p : Parsec α) (acc : Array α) : Nat → (Parsec $ Array α)
  | 0     => pure acc
  | n + 1 => (do repeatCore p (acc.push $ ← p) n)

@[inline]
def repeat' (p : Parsec α) (n : Nat) := repeatCore p #[] n

end Lean.Parsec
end parser_utils
