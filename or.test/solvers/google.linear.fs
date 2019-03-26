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
    let x = Variable.Num "x" 0.0 Double.PositiveInfinity
    let y = Variable.Num "y" 0.0 Double.PositiveInfinity
    let z = Variable.Num "z" 0.0 Double.PositiveInfinity

    let opts =
      SolverParams.Default
      |> DecisionVars [x; y; z]
      |> Goal Maximize
      |> Objective  (10.0*x + 6.0*y + 4.0*z)
      |> Constraint (1.0*x + 1.0*y + 1.0*z <== 100.0)
      |> Constraint (10.0*x + 4.0*y + 5.0*z <== 600.0)
      |> Constraint (2.0*x + 2.0*y + 6.0*z <== 300.0)

    let sol = Solve opts

    sol.Optimal |> should be True