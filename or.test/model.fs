namespace Operations.Research.Test

module ``Models`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models


  [<Fact>]
  let ``create model with decision variables``() =
    let mdl = Model.Default

    let x = Variable.boolean "a" |> toExpression
    let y = Variable.boolean "b" |> toExpression

    let newMdl = mdl |> DecisionVars [x; y]
    newMdl.Variables |> should haveLength 2

  [<Fact>]
  let ``create model with constraints``() =
    let mdl = Model.Default

    let x = Variable.real "a" 0.0 20.0 |> toExpression
    let y = Variable.real "b" -4.0 100.0 |> toExpression
    let c1 = 1.0*x + (-3.4*y) <== 4.5
    let c2 = 3.0*x + 4.9*y >== 50.0
    let c3 = 1.0*x + 1.0*y + 6.0 === 5.0

    let newMdl = mdl |> Constraint c1 |> Constraint c2 |> Constraint c3
    newMdl.Constraints |> should haveLength 3

  [<Fact>]
  let ``create model with objective function expression``() =
    let mdl = Model.Default
    let x = Variable.real "a" 0.0 20.0 |> toExpression
    let y = Variable.real "b" -4.0 100.0 |> toExpression
    let obj = 1.0*x + 0.4*y

    let newMdl = mdl |> Objective obj
    newMdl.Objective.Value |> should be instanceOfType<Expression>

  [<Fact>]
  let ``create default solver options should have default values of none`` () =
    let mdl = Model.Default
    mdl.Variables |> should haveLength 0
    mdl.Objective |> should equal None
    mdl.Constraints |> should haveLength 0

  [<Fact>]
  let ``create constraint with less than or equal operator``()=
    let x = Variable.real "x" 0. 1. |> toExpression
    let c = 1.0*x <== 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with greater than or equal operator``()=
    let x = Variable.real "x" 0. 1. |> toExpression
    let c = 1.0*x >== 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with equal operator``()=
    let x = Variable.real "x" 0. 1. |> toExpression
    let c = 1.0*x === 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with not equal operator throws error if boundary value is not an integer``()=
    let x = Variable.real "x" 0. 1. |> toExpression
    Assert.Throws<Exception>( fun () ->
        1*x =/= 2.0 |> ignore
    )
