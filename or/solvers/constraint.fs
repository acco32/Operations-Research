namespace Operations.Research.Solvers

module Constraint =
  open System
  open Operations.Research.Types
  open Operations.Research.Models


  type SolverOptions = {
    /// Time limit for solver to run. Unit is seconds.
    TimeLimit: int;
  }
  with
  /// Default options to pass to solver
  static member Default =
    { TimeLimit=30 }
  end

  let SolveWithCustomOptions (mdl:Model) (opts:SolverOptions) : SolverResult =

    Solution({ Variables = Map.empty ; Objective = Number.Integer(0); Optimal = true})

  let Solve (mdl:Model) : SolverResult =
    SolveWithCustomOptions mdl SolverOptions.Default

