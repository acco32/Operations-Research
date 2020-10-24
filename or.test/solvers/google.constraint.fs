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
    let r = Variable.Integer("r", 0, 100)
    let p = Variable.Integer("p", 0, 100)

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
        sol.Variables.[r.Name].Name |> should equal r.Name
        sol.Variables.[r.Name].Data.toInt |> should equal 8

        sol.Variables.[p.Name].Name |> should equal p.Name
        sol.Variables.[p.Name].Data.toInt |> should equal 12
    | Error e ->
        Assert.True(false, sprintf "%A" e)

  [<Fact>]
  let ``Solve with 10 second time limit``() =
    let x = Variable.Integer("x", 0, 2)
    let y = Variable.Integer("y", 0, 2)
    let z = Variable.Integer("z", 0, 2)

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
        sol.Variables.[x.Name].Name |> should equal x.Name
        sol.Variables.[x.Name].Data.toInt |> should equal 1

        sol.Variables.[y.Name].Name |> should equal y.Name
        sol.Variables.[y.Name].Data.toInt |> should equal 0

        sol.Variables.[z.Name].Name |> should equal z.Name
        sol.Variables.[z.Name].Data.toInt |> should equal 0
    | Error e ->
        Assert.True(false, sprintf "%A" e)


