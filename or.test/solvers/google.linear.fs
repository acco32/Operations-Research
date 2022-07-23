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
    let x = Variable.real "x" 0.0 1.0 |> toExpression
    let y = Variable.real "y" 0.0 2.0 |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1.0*x + 1.0*y)

    let result = Solve mdl

    match result with
    | Solution sol ->
        sol.Optimal |> should be True
        sol.Objective.toFloat |> should equal 3.0
    | Error e ->
        Assert.True(false, sprintf "%A" e)



  [<Fact>]
  let ``basic linear program with constant variable in Objective``() =
    let x = Variable.integer "x" 0 1 |> toExpression
    let y = Variable.integer "y" 0 2 |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1*x + 1*y + 77)

    let result = Solve mdl

    match result with
    | Solution sol ->
        sol.Optimal |> should be True
        sol.Objective.toInt |> should equal 80
    | Error e ->
        Assert.True(false, sprintf "%A" e)


  [<Fact>]
  let ``basic linear program with boolean variable where we maximize``() =
    let x = Variable.boolean "x" |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x]
      |> Goal Maximize
      |> Objective  (1.0*x + 2.0)

    let result = Solve mdl

    match result with
    | Solution sol ->
        sol.Optimal |> should be True
        sol.Objective.toFloat |> should equal 3.0
    | Error e ->
        Assert.True(false, sprintf "%A" e)

  [<Fact>]
  let ``basic linear program with boolean variable where we minimize``() =
    let x = Variable.boolean "x" |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x]
      |> Goal Minimize
      |> Objective  (1.0*x + 2.0)

    let result = Solve mdl
    match result with
    | Solution sol ->
        sol.Optimal |> should be True
        sol.Objective.toFloat |> should equal 2.0
    | Error e ->
        Assert.True(false, sprintf "%A" e)


  [<Fact>]
  let ``basic linear program in matrix form``() =
    let x = Variable.real "x" 0.0 Double.PositiveInfinity |> toExpression
    let y = Variable.realDefault "y" |> toExpression

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

    match result with
    | Solution sol ->
        sol.Optimal |> should be True

        sol.Variables.ContainsKey(x.var().Name) |> should be True
        sol.Variables.[x.var().Name] |> should (equalWithin 0.001) 44.0

        sol.Variables.ContainsKey(y.var().Name)|> should be True
        sol.Variables.[y.var().Name] |> should (equalWithin 0.001) 16.0

        sol.Objective.toFloat |> should equal 440.0
    | Error e ->
        Assert.True(false, sprintf "%A" e)




  [<Fact>]
  let ``linear program in matrix form - equality constraints``() =
    let x = Variable.real "x" 0.0 Double.PositiveInfinity |> toExpression
    let y = Variable.real "y" 0.0 Double.PositiveInfinity |> toExpression
    let s1 = Variable.real "s1" 0.0 Double.PositiveInfinity |> toExpression
    let s2 = Variable.real "s2" 0.0 Double.PositiveInfinity |> toExpression
    let s3 = Variable.real "s3" 0.0 Double.PositiveInfinity |> toExpression

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

    match result with
    | Solution sol ->
        sol.Objective.toFloat |> should (equalWithin 0.001) 2.4

        sol.Variables.ContainsKey(x.var().Name) |> should be True
        sol.Variables.[x.var().Name] |> should (equalWithin 0.001) 0.6

        sol.Variables.ContainsKey(y.var().Name) |> should be True
        sol.Variables.[y.var().Name] |> should (equalWithin 0.001) 1.2

        sol.Variables.ContainsKey(s1.var().Name) |> should be True
        sol.Variables.[s1.var().Name] |> should (equalWithin 0.001) 0.0

        sol.Variables.ContainsKey(s2.var().Name) |> should be True
        sol.Variables.[s2.var().Name] |> should (equalWithin 0.001) 0.0

        sol.Variables.ContainsKey(s3.var().Name) |> should be True
        sol.Variables.[s3.var().Name] |> should (equalWithin 0.001) 1.0

    | Error e ->
        Assert.True(false, sprintf "%A" e)



  [<Fact>]
  let ``linear program with infeasible/inconsistent results``() =
    let x = Variable.real "x" 0.0 Double.PositiveInfinity |> toExpression
    let y = Variable.real "y" 0.0 Double.PositiveInfinity |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Minimize
      |> Objective  (-2.0*x + 3.0*y)
      |> Constraint (-1.0*x + 2.0*y <== 2.0)
      |> Constraint (2.0*x + -1.0*y <== 3.0)
      |> Constraint (1.0*y >== 4.0)

    let result = Solve mdl

    match result with
    | Solution sol ->
        Assert.True(false, "No solution should be feasible. Something is horribly wrong.")
    | Error e ->
        e.Code |> should equal 2


  [<Fact>]
  let ``integer program``() =
    let x = Variable.integerDefault "x" |> toExpression
    let y = Variable.integerDefault "y" |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x; y]
      |> Goal Maximize
      |> Objective  (1*x + 5*y)
      |> Constraints [
        x + y <== 8
        (-1*x) + 3*y <== 0
      ]

    let opts = { SolverOptions.Default with Strategy=IntegerSolverStrategy.CBC }
    let result = SolveWithCustomOptions mdl opts

    match result with
    | Solution sol ->
        sol.Objective.toInt |> should equal 16

        sol.Variables.ContainsKey(x.var().Name) |> should be True
        sol.Variables.[x.var().Name] |> should equal 6.0

        sol.Variables.ContainsKey(y.var().Name) |> should be True
        sol.Variables.[y.var().Name] |> should equal 2.0

    | Error e ->
        Assert.True(false, sprintf "%A" e)



  [<Fact>]
  let ``linear program with constant in objective function``() =
    let x = Variable.real "x" 0.0 Double.PositiveInfinity |> toExpression
    let y = Variable.real "y" 0.0 Double.PositiveInfinity |> toExpression

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

    match result with
    | Solution sol ->
        sol.Objective.toFloat |> should (equalWithin 0.001) 173.0

        sol.Variables.ContainsKey(x.var().Name) |> should be True
        sol.Variables.[x.var().Name] |> should (equalWithin 0.001) 16.0

        sol.Variables.ContainsKey(y.var().Name) |> should be True
        sol.Variables.[y.var().Name] |> should (equalWithin 0.001) 0.0

    | Error e ->
        Assert.True(false, sprintf "%A" e)



  [<Fact>]
  let ``maximum flow problem as linear program entered as matrix``()=

    let arc01 = Variable.real "arc01" 0.0 3.0 |> toExpression
    let arc02 = Variable.real "arc02" 0.0 2.0 |> toExpression
    let arc03 = Variable.real "arc03" 0.0 2.0 |> toExpression
    let arc14 = Variable.real "arc14" 0.0 5.0 |> toExpression
    let arc15 = Variable.real "arc15" 0.0 1.0 |> toExpression
    let arc24 = Variable.real "arc24" 0.0 1.0 |> toExpression
    let arc25 = Variable.real "arc25" 0.0 3.0 |> toExpression
    let arc26 = Variable.real "arc26" 0.0 1.0 |> toExpression
    let arc35 = Variable.real "arc35" 0.0 1.0 |> toExpression
    let arc47 = Variable.real "arc47" 0.0 4.0 |> toExpression
    let arc57 = Variable.real "arc57" 0.0 2.0 |> toExpression
    let arc67 = Variable.real "arc67" 0.0 4.0 |> toExpression

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

    match result with
    | Solution sol ->
        sol.Objective.toFloat |> should (equalWithin 0.001) 6.0
        sol.Variables.["arc01"] |> should (equalWithin 0.001) 3.0
        sol.Variables.["arc02"] |> should (equalWithin 0.001) 2.0
        sol.Variables.["arc03"] |> should (equalWithin 0.001) 1.0
        sol.Variables.["arc14"] |> should (equalWithin 0.001) 3.0
        sol.Variables.["arc15"] |> should (equalWithin 0.001) 0.0
        sol.Variables.["arc24"] |> should (equalWithin 0.001) 1.0
        sol.Variables.["arc25"] |> should (equalWithin 0.001) 0.0
        sol.Variables.["arc26"] |> should (equalWithin 0.001) 1.0
        sol.Variables.["arc35"] |> should (equalWithin 0.001) 1.0
        sol.Variables.["arc47"] |> should (equalWithin 0.001) 4.0
        sol.Variables.["arc57"] |> should (equalWithin 0.001) 1.0
        sol.Variables.["arc67"] |> should (equalWithin 0.001) 1.0
    | Error e ->
        Assert.True(false, sprintf "%A" e)



  [<Fact>]
  let ``linear program with disjunctive constraint throws error``()=
    let x = Variable.integer "x" -6 6 |> toExpression
    let y = Variable.integer "y" -6 6 |> toExpression

    Assert.Throws<Exception>(fun() ->
      let mdl = Model.Default
                  |> DecisionVars [x; y]
                  |> Goal Maximize
                  |> Objective (-0.5*x + 1*y + -2)
                  |> Constraints [
                    1*x =/= 2
                  ]
      let opts = { SolverOptions.Default with Strategy=IntegerSolverStrategy.CBC }
      let result = SolveWithCustomOptions mdl opts

      printf "Should have thrown error but got to this message instead."
    )
