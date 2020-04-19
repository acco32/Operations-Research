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
