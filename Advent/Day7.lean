import Advent.Util
import Std.Data.HashMap

inductive Cmd
  | cd (dir : String)
  | cdUp
  | ls
deriving Repr, Inhabited

instance : ToString Cmd := ⟨reprStr⟩

inductive DirItem
  | dir (name : String) 
  | file (name : String) (size : Nat)
deriving Repr, Inhabited

instance : ToString DirItem := ⟨reprStr⟩

section parsing

open Lean Parsec

def parseCd : Parsec Cmd := 
  skipString "$ cd " *> parseName >>= fun name => pure $ .cd name

def parseLs : Parsec Cmd :=
  skipString "$ ls" *> pure .ls

def parseCdUp : Parsec Cmd :=
  skipString "$ cd .." *> pure .cdUp

def parseCmd : Parsec Cmd := parseLs <|> parseCdUp <|> parseCd

def parseFile : Parsec DirItem := do
  let size ← parseNum
  ws
  let name ← parseName
  return .file name size

def parseDir : Parsec DirItem := skipString "dir " *> parseName >>= fun name => pure $ .dir name

def parseDirItem : Parsec DirItem := parseFile <|> parseDir

def parseInput (input : Array String) : Array (Cmd ⊕ DirItem) := Id.run do
  let mut answer := #[]
  for line in input do
    match parseCmd.run line with
    | .ok cmd => 
      answer := answer.push (.inl cmd)
    | .error _ => 
      match parseDirItem.run line with
      | .ok item => answer := answer.push (.inr item)
      | .error _ => ()
  return answer

end parsing

section logic

open Std

abbrev DirPath := String

abbrev Context := Array (Cmd ⊕ DirItem)

structure State where
  idx : Nat
  root : List DirPath
  dirSizes : HashMap DirPath Nat
deriving Inhabited

def init : State := default

abbrev BuildM := ReaderT Context $ StateM State

def getn (n : Nat) : BuildM (Cmd ⊕ DirItem) := do
  let ctx ← read
  return ctx[n]!

def idx : BuildM Nat := do
  return (← get).idx

def readIdx : BuildM $ Array (Cmd ⊕ DirItem) := do
  let ctx ← read
  let pos ← idx
  return ctx.extract pos (ctx.size)

def incIdx (inc : Nat) : BuildM Unit := do
  let old ← idx
  modify (fun st => {st with idx := old + inc})

def getLines : BuildM (List DirItem) := do
  let mut answer := []
  let lines ← readIdx
  for line in lines do
    match line with
    | .inr item => answer := answer.cons item
    | .inl _ => break
  incIdx (answer.length)
  return answer
  
def buildTree : BuildM Unit := do
  sorry
  
end logic

def day7 : IO Unit := do
  let inputLines ← getInputLines (test := true) (day := 7)
  dbg_trace parseInput inputLines

  printAnswer (day := 7) 0 0

#eval day7