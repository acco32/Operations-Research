namespace Operations.Research.Test

module ``Google Solver - Linear`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google.Linear


  [<Fact>]
  let ``basic linear program``() =
    let x = Variable.Real("x", 0.0, 1.0)
    let y = Variable.Real("y", 0.0, 2.0)

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1.0*x + 1.0*y)

    let result = Solve mdl

    result.Sol.Optimal |> should be True
    result.Sol.Objective |> should equal 3.0

  [<Fact>]
  let ``basic linear program with constant variable in Objective``() =
    let x = Variable.Integer("x", 0, 1)
    let y = Variable.Integer("y", 0, 2)

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1.0*x + 1.0*y + 77.0)

    let result = Solve mdl
    result.Sol.Optimal |> should be True
    result.Sol.Objective |> should equal 80.0

  [<Fact>]
  let ``basic linear program with boolean variable where we maximize``() =
    let x = Variable.Bool "x"

    let mdl =
      Model.Default
      |> DecisionVars [x]
      |> Goal Maximize
      |> Objective  (1.0*x + 2.0)

    let result = Solve mdl
    result.Sol.Optimal |> should be True
    result.Sol.Objective |> should equal 3.0

  [<Fact>]
  let ``basic linear program with boolean variable where we minimize``() =
    let x = Variable.Bool "x"

    let mdl =
      Model.Default
      |> DecisionVars [x]
      |> Goal Minimize
      |> Objective  (1.0*x + 2.0)

    let result = Solve mdl
    result.Sol.Optimal |> should be True
    result.Sol.Objective |> should equal 2.0

  [<Fact>]
  let ``basic linear program in matrix form``() =
    let x = Variable.Real("x", 0.0, Double.PositiveInfinity)
    let y = Variable.Real("y")

    let m = [[2.0; 1.0]; [1.0; 2.0]]
    let lb = [0.0; 0.0]
    let ub = [104.0; 76.0]

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (6.0*x + y*11.0)
      |> Matrix m lb ub

    let result = Solve mdl

    result.Sol.Optimal |> should be True

    result.Sol.Variables.["x"].Name |> should equal x.Name
    result.Sol.Variables.["x"].Data.Number.toFloat |> should (equalWithin 0.001) 44.0

    result.Sol.Variables.["y"].Name |> should equal y.Name
    result.Sol.Variables.["y"].Data.Number.toFloat |> should (equalWithin 0.001) 16.0

    result.Sol.Objective |> should equal 440.0

  [<Fact>]
  let ``linear program in matrix form - equality constraints``() =
    let x = Variable.Real("x", 0.0, Double.PositiveInfinity)
    let y = Variable.Real("y", 0.0, Double.PositiveInfinity)
    let s1 = Variable.Real("s1", 0.0, Double.PositiveInfinity)
    let s2 = Variable.Real("s2", 0.0, Double.PositiveInfinity)
    let s3 = Variable.Real("s3", 0.0, Double.PositiveInfinity)

    let m = [
      [3.0 ; 1.0 ; -1.0 ; 0.0 ; 0.0];
      [4.0 ; 3.0 ; 0.0 ; -1.0 ; 0.0];
      [1.0 ; 2.0 ; 0.0 ; 0.0 ; -1.0]
    ]

    let eq = [3.0; 6.0; 2.0]

    let mdl =
          Model.Default
          |> DecisionVars [x; y; s1; s2; s3]
          |> Goal Minimize
          |> Objective  (2.0*x + 1.0*y)
          |> MatrixEq m eq

    let result = Solve mdl
    result.Sol.Objective |> should (equalWithin 0.001) 2.4

    result.Sol.Variables.["x"].Name |> should equal x.Name
    result.Sol.Variables.["x"].Data.Number.toFloat |> should (equalWithin 0.001) 0.6

    result.Sol.Variables.["y"].Name |> should equal y.Name
    result.Sol.Variables.["y"].Data.Number.toFloat |> should (equalWithin 0.001) 1.2

    result.Sol.Variables.["s1"].Name |> should equal s1.Name
    result.Sol.Variables.["s1"].Data.Number.toFloat |> should (equalWithin 0.001) 0.0

    result.Sol.Variables.["s2"].Name |> should equal s2.Name
    result.Sol.Variables.["s2"].Data.Number.toFloat |> should (equalWithin 0.001) 0.0

    result.Sol.Variables.["s3"].Name |> should equal s3.Name
    result.Sol.Variables.["s3"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0

  [<Fact>]
  let ``linear program with infeasible/inconsistent results``() =
    let x = Variable.Real("x", 0.0, Double.PositiveInfinity)
    let y = Variable.Real("y", 0.0, Double.PositiveInfinity)

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Minimize
      |> Objective  (-2.0*x + 3.0*y)
      |> Constraint (-1.0*x + 2.0*y <== 2.0)
      |> Constraint (2.0*x + -1.0*y <== 3.0)
      |> Constraint (1.0*y >== 4.0)

    let result = Solve mdl
    result.Err.Code |> should equal 2

  // [<Fact>]
  // let ``integer program with constant in objective function``() =
  //   let x = Variable.Integer("x")
  //   let y = Variable.Integer("y")

  //   let mdl =
  //     Model.Default
  //     |> DecisionVars [x; y]
  //     |> Goal Maximize
  //     |> Objective  (6*x + 2*y + 77)
  //     |> Constraints [
  //       3*x + 1*y <== 48
  //       3*x + 4*y <== 120
  //       3*x + 1*y >== 36
  //     ]

  //   let opts = { SolverOptions.Default with Strategy=IntegerSolverStrategy.CBC }

  //   let result = SolveWithCustomOptions mdl opts
  //   result.Sol.Objective |> should (equalWithin 0.001) 173

  //   result.Sol.Variables.["x"].Name |> should equal x.Name
  //   result.Sol.Variables.["x"].Data.Number.toFloat |> should (equalWithin 0.001) 8.0

  //   result.Sol.Variables.["y"].Name |> should equal y.Name
  //   result.Sol.Variables.["y"].Data.Number.toFloat |> should (equalWithin 0.001) 24.0

  [<Fact>]
  let ``linear program with constant in objective function``() =
    let x = Variable.Real("x", 0.0, Double.PositiveInfinity)
    let y = Variable.Real("y", 0.0, Double.PositiveInfinity)

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (6.0*x + 2.0*y + 77.0)
      |> Constraints [
        3.0*x + 1.0*y <== 48.0
        3.0*x + 4.0*y <== 120.0
        3.0*x + 1.0*y >== 36.0
      ]

    let result = SolveWithCustomOptions mdl SolverOptions.Default
    result.Sol.Objective |> should (equalWithin 0.001) 173.0

    result.Sol.Variables.["x"].Name |> should equal x.Name
    result.Sol.Variables.["x"].Data.Number.toFloat |> should (equalWithin 0.001) 16.0

    result.Sol.Variables.["y"].Name |> should equal y.Name
    result.Sol.Variables.["y"].Data.Number.toFloat |> should (equalWithin 0.001) 0.0

  [<Fact>]
  let ``maximum flow problem as linear program entered as matrix``()=

    let arc01 = Variable.Real("arc01", 0.0, 3.0)
    let arc02 = Variable.Real("arc02", 0.0, 2.0)
    let arc03 = Variable.Real("arc03", 0.0, 2.0)
    let arc14 = Variable.Real("arc14", 0.0, 5.0)
    let arc15 = Variable.Real("arc15", 0.0, 1.0)
    let arc24 = Variable.Real("arc24", 0.0, 1.0)
    let arc25 = Variable.Real("arc25", 0.0, 3.0)
    let arc26 = Variable.Real("arc26", 0.0, 1.0)
    let arc35 = Variable.Real("arc35", 0.0, 1.0)
    let arc47 = Variable.Real("arc47", 0.0, 4.0)
    let arc57 = Variable.Real("arc57", 0.0, 2.0)
    let arc67 = Variable.Real("arc67", 0.0, 4.0)

    let m = [
        [1.0; 0.0; 0.0; -1.0; -1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0];
        [0.0; 1.0; 0.0; 0.0; 0.0; -1.0; -1.0; -1.0; 0.0; 0.0; 0.0; 0.0];
        [0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; -1.0; 0.0; 0.0; 0.0];
        [0.0; 0.0; 0.0; 1.0; 0.0; 1.0; 0.0; 0.0; 0.0; -1.0; 0.0; 0.0];
        [0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 1.0; 0.0; 1.0; 0.0; -1.0; 0.0];
        [0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; -1.0];
      ]

    let b = Seq.replicate 6 0.0 |> Seq.toList

    let mdl =
      Model.Default
      |> DecisionVars [arc01; arc02; arc03; arc14; arc15; arc24; arc25; arc26; arc35; arc47; arc57; arc67]
      |> Goal Maximize
      |> Objective  (1.0*arc01 + 1.0*arc02 + 1.0*arc03)
      |> MatrixEq m b

    let result = Solve mdl

    result.Sol.Objective |> should (equalWithin 0.001) 6.0
    result.Sol.Variables.["arc01"].Data.Number.toFloat |> should (equalWithin 0.001) 3.0
    result.Sol.Variables.["arc02"].Data.Number.toFloat |> should (equalWithin 0.001) 2.0
    result.Sol.Variables.["arc03"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0
    result.Sol.Variables.["arc14"].Data.Number.toFloat |> should (equalWithin 0.001) 3.0
    result.Sol.Variables.["arc15"].Data.Number.toFloat |> should (equalWithin 0.001) 0.0
    result.Sol.Variables.["arc24"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0
    result.Sol.Variables.["arc25"].Data.Number.toFloat |> should (equalWithin 0.001) 0.0
    result.Sol.Variables.["arc26"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0
    result.Sol.Variables.["arc35"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0
    result.Sol.Variables.["arc47"].Data.Number.toFloat |> should (equalWithin 0.001) 4.0
    result.Sol.Variables.["arc57"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0
    result.Sol.Variables.["arc67"].Data.Number.toFloat |> should (equalWithin 0.001) 1.0


