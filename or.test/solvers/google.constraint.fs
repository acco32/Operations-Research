namespace Operations.Research.Test

module ``Google Solver - Constraint`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google.Constraint

  [<Fact>]
  let ``Rabbits/Pheasants``() =
    let r = Variable.integer "r" 0 100 |> toExpression
    let p = Variable.integer "p" 0 100 |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [r; p]
      |> Constraints [
        r + p === 20
        4*r + 2*p === 56
      ]

    let result = Solve mdl

    match result with
    | Solution sol ->

        sol.Variables.ContainsKey(r.var().Name) |> should be True
        sol.Variables.[r.var().Name] |> should equal 8.0

        sol.Variables.ContainsKey(p.var().Name) |> should be True
        sol.Variables.[p.var().Name] |> should equal 12.0

    | Error e ->
        Assert.True(false, sprintf "%A" e)

  [<Fact>]
  let ``Solve with 10 second time limit``() =
    let x = Variable.integer "x" 0 2 |> toExpression
    let y = Variable.integer "y" 0 2 |> toExpression
    let z = Variable.integer "z" 0 2 |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x; y; z]
      |> Constraints [
        x + -1*y =/= 0
      ]

    let solverOpts = { SolverOptions.Default with TimeLimit = 10 }
    let result = SolveWithCustomOptions mdl solverOpts

    match result with
    | Solution sol ->
        sol.Variables.ContainsKey(x.var().Name) |> should be True
        sol.Variables.[x.var().Name] |> should equal 1.0

        sol.Variables.ContainsKey(y.var().Name) |> should be True
        sol.Variables.[y.var().Name] |> should equal 0.0

        sol.Variables.ContainsKey(z.var().Name) |> should be True
        sol.Variables.[z.var().Name] |> should equal 0.0

    | Error e ->
        Assert.True(false, sprintf "%A" e)


