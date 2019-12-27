namespace Operations.Research.Test

module ``Models2`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types2
  open Operations.Research.Models2


  [<Fact>]
  let ``create model with decision variables``() =
    let mdl = Model.Default

    let x = Variable.Bool "a"
    let y = Variable.Bool "b"

    let newMdl = mdl |> DecisionVars [x; y]
    newMdl.Variables |> should haveLength 2

  [<Fact>]
  let ``create model with constraints``() =
    let mdl = Model.Default

    let x = Variable.Real("a", 0.0, 20.0)
    let y = Variable.Real("b", -4.0, 100.0)
    let c1 = 1.0*x + (-3.4*y) <== 4.5
    let c2 = 3.0*x + 4.9*y >== 50.0
    let c3 = 1.0*x + 1.0*y + 6.0 === 5.0

    let newMdl = mdl |> Constraint c1 |> Constraint c2 |> Constraint c3
    newMdl.Constraints |> should haveLength 3

  [<Fact>]
  let ``create model with objective function expression``() =
    let mdl = Model.Default
    let x = Variable.Real("a", 0.0, 20.0)
    let y = Variable.Real("b", -4.0, 100.0)
    let obj = 1.0*x + 0.4*y

    let newMdl = mdl |> Objective obj
    newMdl.Objective.Value |> should be instanceOfType<Operand>


