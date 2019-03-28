namespace Operations.Research.Test

module ``Google Solver`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google


  [<Fact>]
  let ``basic linear program``() =
    let x = Variable.Num "x" 0.0 1.0
    let y = Variable.Num "y" 0.0 2.0

    let opts =
      SolverParams.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1.0*x + 1.0*y)

    let sol = Solve opts

    sol.Optimal |> should be True
    sol.Objective |> should equal 3.0
    sol.Error |> should equal None