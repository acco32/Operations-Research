namespace Operations.Research.Test

module ``Google Solver - Constraint`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google.Constraint


  // [<Fact>]
  let ``basic constraint program``() =
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
        Assert.True(true)
    | Error e ->
        Assert.True(false, sprintf "%A" e)

