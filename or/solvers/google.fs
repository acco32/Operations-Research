namespace Operations.Research


module GoogleSolver =
  open Operations.Research.Types
  open Google.OrTools.LinearSolver

  let Solve (opts:SolverParams) : Solver =
    null