import Lake
open Lake DSL

package advent

@[default_target]
lean_lib Advent 

lean_exe advent {
  root := `Main
}
