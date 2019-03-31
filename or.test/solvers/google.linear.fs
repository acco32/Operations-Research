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

  [<Fact>]
  let ``basic linear program with constant variable in Objective``() =
    let x = Variable.Num "x" 0.0 1.0
    let y = Variable.Num "y" 0.0 2.0

    let opts =
      SolverParams.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1.0*x + 1.0*y + 77.0)

    let sol = Solve opts

    sol.Optimal |> should be True
    sol.Objective |> should equal 80.0
    sol.Error |> should equal None

  [<Fact>]
  let ``basic linear program with boolean variable where we maximize``() =
    let x = Variable.Bool "x"

    let opts =
      SolverParams.Default
      |> DecisionVars [x]
      |> Goal Maximize
      |> Objective  (1.0*x + 2.0)

    let sol = Solve opts

    sol.Optimal |> should be True
    sol.Objective |> should equal 3.0
    sol.Error |> should equal None

  [<Fact>]
  let ``basic linear program with boolean variable where we minimize``() =
    let x = Variable.Bool "x"

    let opts =
      SolverParams.Default
      |> DecisionVars [x]
      |> Goal Minimize
      |> Objective  (1.0*x + 2.0)

    let sol = Solve opts

    sol.Optimal |> should be True
    sol.Objective |> should equal 2.0
    sol.Error |> should equal None



