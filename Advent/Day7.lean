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

namespace Day7

open Std

abbrev DirPath := String

abbrev Context := Array (Cmd ⊕ DirItem)

structure State where
  idx : Nat
  root : List DirPath
  dirSizes : HashMap (List DirPath) Nat
deriving Inhabited

def init : State := {
  idx := 0
  root := []
  dirSizes := .empty |>.insert [] 0
}

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

def getLines : BuildM (Array DirItem) := do
  let lines ← readIdx
  let gotLines := lines.takeWhile (·.isRight) |>.map Sum.projectRight
  incIdx (gotLines.size)
  return gotLines

def readCommand : BuildM $ Option Cmd := do
  let pos ← idx
  let ctx ← read
  match ctx[pos]? with
  | .some (.inl cmd) => return .some cmd
  | .some (.inr _) => panic! "Uh oh! This shouldn't happen"
  | .none => return .none

def getParents (currentDir : List DirPath) : List (List DirPath) :=
  let rec getParentsAux (acc : List (List DirPath)) (currentDir : List DirPath) : List (List DirPath) :=
    match currentDir with
    | [] => acc
    | _ :: xs => getParentsAux (xs :: acc) xs
  getParentsAux [currentDir] currentDir

partial def updateParents (childSize : Nat) : BuildM Unit := do
  let currentDir := (← get).root
  for parent in getParents currentDir do
    modify fun st => 
    { st with
      dirSizes := st.dirSizes.insert parent (st.dirSizes.find! parent + childSize)
    }

partial def buildTree : BuildM Unit := do
  let cmd? ← readCommand
  incIdx 1
  match cmd? with
  | .some $ .cd dirName => 
    modify fun st => {
      st with
      root := dirName :: st.root
      dirSizes := st.dirSizes.insert (dirName :: st.root) 0
    }
    buildTree
  | .some $ .cdUp => 
    modify fun st => {
      st with
      root := st.root.tail
    }
    buildTree
  | .some .ls =>
    let items ← getLines
    let mut totalSize := 0
    for item in items do
      match item with
      | .dir _ => pure ()
      | .file _ size => 
        totalSize := totalSize + size
    updateParents totalSize
    buildTree
  | .none => pure ()
  
end Day7
end logic

open Day7

def day7 : IO Unit := do
  let inputLines ← getInputLines (test := false) (day := 7)
  let parsedInputs := parseInput inputLines
  let dirSizes := buildTree.run parsedInputs |>.run default |>.snd |>.dirSizes
  let answer1 := dirSizes |>.filter (fun _ size => size ≤ 100000)
                          |>.fold (fun acc _ c => acc + c) (init := 0)
  let unusedSpace := 70000000 - dirSizes.find! []
  let spaceNeeded := 30000000 - unusedSpace
  let answer2 := dirSizes |>.filter (fun _ size => size ≥ spaceNeeded)
                          |>.toArray
                          |>.qsort (fun (_, n) (_, m) => n < m)
                          |>.get! 0
                          |>.snd
  printAnswer (day := 7) answer1 answer2

#eval day7