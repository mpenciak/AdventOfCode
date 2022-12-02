import Advent.Util

inductive RPS | rock | paper | scissors
deriving BEq, Repr

inductive GameResult | pOne | pTwo | draw
deriving Repr

def runGame : RPS → RPS → GameResult
  | .rock    , .scissors => .pOne
  | .paper   , .rock  => .pOne
  | .scissors, .paper => .pOne
  | a        , b      => if a == b then .draw else .pTwo

def scoreMove : RPS → Nat 
  | .rock     => 1
  | .paper    => 2
  | .scissors => 3

def scoreResult : GameResult → Nat
  | .pOne => 0
  | .draw => 3
  | .pTwo => 6

def scoreGame (move : RPS) (res : GameResult) : Nat := scoreMove move + scoreResult res

def String.toMove : String → RPS
  | "A" | "X" => .rock
  | "B" | "Y" => .paper
  | "C" | "Z" => .scissors
  | _ => .rock -- dummy stuff because I'm lazy

def String.toResult : String → GameResult
  | "X" => .pOne
  | "Y" => .draw
  | "Z" => .pTwo
  | _ => .pOne -- dummy stuff because I'm lazy

def figureMove : RPS → GameResult → RPS
  | mov, .draw => mov
  | .rock, .pOne => .scissors
  | .paper, .pOne => .rock
  | .scissors, .pOne => .paper
  | .rock, .pTwo => .paper
  | .paper, .pTwo => .scissors
  | .scissors, .pTwo => .rock 

def day2 : IO Unit := do
  let inputLines ← getInputLines (test := false) 2
  let parsedInput1  := inputLines |>.map (fun s => s.split' (· == ' '))
                                  |>.map (fun (s₁, s₂) => (s₁.toMove, s₂.toMove))
                                  |>.map (fun (m₁, m₂) => (m₂, runGame m₁ m₂))
  let answer1 := parsedInput1 |>.map (fun (mov, res) => scoreGame mov res)
                              |>.sum

  let parsedInput2 := inputLines |>.map (fun s => s.split' (· == ' '))
                                 |>.map (fun (s₁, s₂) => (s₁.toMove, s₂.toResult))
                                 |>.map (fun (m₁, res) => (figureMove m₁ res, res))
  let answer2 := parsedInput2 |>.map (fun (mov, res) => scoreGame mov res)
                              |>.sum
  -- dbg_trace parsedInput2.map (reprStr)
  printAnswer (day := 2) answer1 answer2

#eval day2
