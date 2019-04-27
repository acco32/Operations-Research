namespace Operations.Research.Test

module ``Models`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models


  [<Fact>]
  let ``create model with decision variables``() =
    let opts = SolverParams.Default

    let x = Variable.Bool "a"
    let y = Variable.Bool "b"

    let newOpts = opts |> DecisionVars [x; y]
    newOpts.Variables |> should haveLength 2

  [<Fact>]
  let ``create model with constraints``() =
    let opts = SolverParams.Default

    let x = Variable.Num "a" 0.0 20.0
    let y = Variable.Num "b" -4.0 100.0
    let c1 = 1.0*x + (-3.4*y) <== 4.5
    let c2 = 3.0*x + 4.9*y >== 50.0
    let c3 = 1.0*x + 1.0*y + 6.0 === 5.0

    let newOpts = opts |> Constraint c1 |> Constraint c2 |> Constraint c3
    newOpts.Constraints |> should haveLength 3

  [<Fact>]
  let ``create model with objective function expression``() =
    let opts = SolverParams.Default
    let x = Variable.Num "a" 0.0 20.0
    let y = Variable.Num "b" -4.0 100.0
    let obj = 1.0*x + 0.4*y

    let newOpts = opts |> Objective obj
    newOpts.Objective.Value |> should be instanceOfType<Operand>


