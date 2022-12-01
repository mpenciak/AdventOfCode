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
  return ans

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

end data_utils

section parser_utils 

open Lean Parsec

def parseDigit : Parsec Nat := attempt do
  let n ← digit
  return n.toDigit

def parseNum : Parsec Nat := 
  many1 parseDigit >>= fun ns => pure $ ns.foldl (fun n m => 10 * n + m) 0

end parser_utils