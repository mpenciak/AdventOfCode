import Lake
open Lake DSL

package advent

@[default_target]
lean_lib Advent 

@[default_target]
lean_exe advent {
  root := `Main
}

require std from git "https://github.com/leanprover/std4" @ "main"