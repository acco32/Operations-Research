namespace Operations.Research.Test

module ``Basic Types 2`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types2
  // open Operations.Research.Models

  [<Fact>]
  let ``create boolean variable with default values``() =
    let varName = "bool"
    let v = Variable2.Bool varName

    v.Data.toInt |> should equal 0
    v.Data.toFloat |> should equal 0.0
    v.State |> should be False
    v.Name |> should equal varName

  [<Fact>]
  let ``create number variable with default values``() =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0

    let v = Variable2.Real(varName, lb, ub)

    v.LowerBound.toFloat |> should equal (lb)
    v.UpperBound.toFloat |> should equal (ub)

    v.Data.toFloat |> should equal (0.0)
    v.Name |> should equal varName

  [<Fact>]
  let ``create boolean operand`` () =
    let x = Variable2.Bool "a"
    let result = 1*x
    result |> should be instanceOfType<Expression2>

  [<Fact>]
  let ``create number (integer) operand`` () =
    let x = Variable2.Integer("a", 0, 1)
    let result = 1*x
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 1


  [<Fact>]
  let ``create number (float) operand`` () =
    let x = Variable2.Real("a", 0., 1.)
    let result = 1*x
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 1


  [<Fact>]
  let ``add two (integer) variables and create an expression``() =
    let x = Variable2.Integer "a"
    let y = Variable2.Integer "b"

    let result = x + y
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 2


  [<Fact>]
  let ``add number variable (integer) and constant should create an expression``() =
    let x = Variable2.Integer "a"

    let result = x + 77
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 2


  [<Fact>]
  let ``add number variable (real) and constant should create an expression``() =
    let x = Variable2.Real "a"

    let result = x + 77.0
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 2


  [<Fact>]
  let ``add number variable (real) and coefficient-variable (real) should create an expression``() =
    let x = Variable2.Real "a"
    let y = Variable2.Real "b"

    let result = x + 2*y
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 2


  [<Fact>]
  let ``create a multi-variable expression from integer variables`` () =
    let x = Variable2.Integer("a", 0, 10)
    let y = Variable2.Integer("b", 0, 10)
    let z = Variable2.Integer("c", 0, 10)

    let result = x + 2*y + 5*z + 80
    result |> should be instanceOfType<Expression2>
    result.Statement |> should haveLength 4


  [<Fact>]
  let ``set number variable`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let mutable v = Variable2.Real(varName, lb, ub)

    let newValue = 1.0
    v.Data.toFloat |> should equal 0.0
    v <- v.Set(newValue)
    v.Data.toFloat |> should equal newValue


  [<Fact>]
  let ``set number variable throws error if out of bounds`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let v = Variable2.Real(varName, lb, ub)

    let outOfBoundsValue = 9.0
    shouldFail (fun () -> v.Set outOfBoundsValue |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set boolean variable throws error if set to number`` () =
    let v = Variable2.Bool "bool"

    let invalidValue = 9.0
    shouldFail (fun () -> v.Set invalidValue |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set number variable throws error if set to boolean`` () =
    let v = Variable2.Integer("num", 0, 1)

    let invalidValue = true
    shouldFail (fun () -> v.Set invalidValue |> should throw typeof<System.ArgumentOutOfRangeException> )

  // [<Fact>]
  // let ``create default solver options should have default values of none`` () =
  //   let mdl = Model.Default
  //   mdl.Variables |> should haveLength 0
  //   mdl.Objective |> should equal None
  //   mdl.Constraints |> should haveLength 0

  // [<Fact>]
  // let ``create constraint with less than or equal operator``()=
  //   let x = Variable.Real("x", 0., 1.)
  //   let c = 1.0*x <== 2.0
  //   c |> should be instanceOfType<Constraint>

  // [<Fact>]
  // let ``create constraint with greater than or equal operator``()=
  //   let x = Variable.Real("x", 0., 1.)
  //   let c = 1.0*x >== 2.0
  //   c |> should be instanceOfType<Constraint>

  // [<Fact>]
  // let ``create constraint with equal operator``()=
  //   let x = Variable.Real("x", 0., 1.)
  //   let c = 1.0*x === 2.0
  //   c |> should be instanceOfType<Constraint>

  // [<Fact>]
  // let ``create constraint with not equal operator throws error if boundary value is not an integer``()=
  //   let x = Variable.Real("x", 0., 1.)
  //   Assert.Throws<Exception>( fun () ->
  //       1*x =/= 2.0 |> ignore
  //   )


