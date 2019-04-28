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
    let x = Variable.Num "x" 0.0 1.0
    let y = Variable.Num "y" 0.0 2.0

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
    let x = Variable.Num "x" 0.0 Double.PositiveInfinity
    let y = Variable.Num "y" 0.0 Double.PositiveInfinity

    let m = [[2.0; 1.0]; [1.0; 2.0]]
    let lb = [0.0; 0.0]
    let ub = [104.0; 76.0]

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (6.0*x + 11.0*y)
      |> Matrix m lb ub

    let result = Solve mdl

    result.Sol.Optimal |> should be True

    result.Sol.Variables.[0].Name |> should equal x.Name
    result.Sol.Variables.[0].Value |> should (equalWithin 0.001) 44.0

    result.Sol.Variables.[1].Name |> should equal y.Name
    result.Sol.Variables.[1].Value |> should (equalWithin 0.001) 16.0

    result.Sol.Objective |> should equal 440.0

  [<Fact>]
  let ``linear program in matrix form - equality constraints``() =
    let x = Variable.Num "x" 0.0 Double.PositiveInfinity
    let y = Variable.Num "y" 0.0 Double.PositiveInfinity
    let s1 = Variable.Num "s1" 0.0 Double.PositiveInfinity
    let s2 = Variable.Num "s2" 0.0 Double.PositiveInfinity
    let s3 = Variable.Num "s3" 0.0 Double.PositiveInfinity

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

    result.Sol.Variables.[0].Name |> should equal x.Name
    result.Sol.Variables.[0].Value |> should (equalWithin 0.001) 0.6

    result.Sol.Variables.[1].Name |> should equal y.Name
    result.Sol.Variables.[1].Value |> should (equalWithin 0.001) 1.2

    result.Sol.Variables.[2].Name |> should equal s1.Name
    result.Sol.Variables.[2].Value |> should (equalWithin 0.001) 0.0

    result.Sol.Variables.[3].Name |> should equal s2.Name
    result.Sol.Variables.[3].Value |> should (equalWithin 0.001) 0.0

    result.Sol.Variables.[4].Name |> should equal s3.Name
    result.Sol.Variables.[4].Value |> should (equalWithin 0.001) 1.0

  [<Fact>]
  let ``linear program with infeasible/inconsistent results``() =
    let x = Variable.Num "x" 0.0 Double.PositiveInfinity
    let y = Variable.Num "y" 0.0 Double.PositiveInfinity

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

  [<Fact>]
  let ``integer program with constant in objective function``() =
    let x = Variable.Num "x" 0.0 Double.PositiveInfinity
    let y = Variable.Num "y" 0.0 Double.PositiveInfinity

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (6.0*x + 2.0*y + 77.0)
      |> Constraint (3.0*x + 1.0*y <== 48.0)
      |> Constraint (3.0*x + 4.0*y <== 120.0)
      |> Constraint (3.0*x + 1.0*y >== 36.0)

    let opts = { SolverOptions.Default with Strategy=IntegerSolverStrategy.CBC }

    let result = SolveWithCustomOptions mdl opts
    result.Sol.Objective |> should (equalWithin 0.001) 173

    result.Sol.Variables.[0].Name |> should equal x.Name
    result.Sol.Variables.[0].Value |> should (equalWithin 0.001) 8.0

    result.Sol.Variables.[1].Name |> should equal y.Name
    result.Sol.Variables.[1].Value |> should (equalWithin 0.001) 24.0

  [<Fact>]
  let ``linear program with constant in objective function``() =
    let x = Variable.Num "x" 0.0 Double.PositiveInfinity
    let y = Variable.Num "y" 0.0 Double.PositiveInfinity

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (6.0*x + 2.0*y + 77.0)
      |> Constraint (3.0*x + 1.0*y <== 48.0)
      |> Constraint (3.0*x + 4.0*y <== 120.0)
      |> Constraint (3.0*x + 1.0*y >== 36.0)

    let result = SolveWithCustomOptions mdl SolverOptions.Default
    result.Sol.Objective |> should (equalWithin 0.001) 173.0

    result.Sol.Variables.[0].Name |> should equal x.Name
    result.Sol.Variables.[0].Value |> should (equalWithin 0.001) 16.0

    result.Sol.Variables.[1].Name |> should equal y.Name
    result.Sol.Variables.[1].Value |> should (equalWithin 0.001) 0.0

