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

  // [<Fact>]
  // let ``create boolean operand`` () =
  //   let x = Variable2.Bool "a"
  //   let result = 1*x
  //   result |> should be instanceOfType<Operand2>

  // [<Fact>]
  // let ``create number (integer) operand`` () =
  //   let x = Variable.Integer("a", 0, 1)
  //   let result = 1*x
  //   result |> should be instanceOfType<Operand>

  // let ``create number (float) operand`` () =
  //   let x = Variable.Real("a", 0., 1.)
  //   let result = 1*x
  //   result |> should be instanceOfType<Operand>

  // [<Fact>]
  // let ``able to create mixed 2-operand expression`` () =
  //   let x = Variable.Integer "a"
  //   let y = Variable.Bool "b"

  //   let op1 = 1.0*x
  //   let op2 = 1.0*y

  //   let result = op1 + op2
  //   result |> should be instanceOfType<Operand>

  //   match result with
  //   | Expression e ->
  //       e |> should haveLength 2
  //   | _ -> Assert.False(true, "Should be an expression of length 2")

  //   // let operands = ops result
  //   // operands |> List.length |> should equal 2
  //   // operands |> List.contains op1 |> should be True
  //   // operands |> List.contains op2 |> should be True


  // [<Fact>]
  // let ``able to create mixed 3-operand expression`` () =
  //   let x = Variable.Integer("a", 0, 1)
  //   let y = Variable.Real("b", 0., 1.)
  //   let z = Variable.Bool("c")

  //   let op1 = 1*x
  //   let op2 = 1*y
  //   let op3 = 1*z

  //   let result = op1 + op2 + op3
  //   result |> should be instanceOfType<Operand>

  //   match result with
  //   | Expression e ->
  //       e |> should haveLength 3
  //   | _ -> Assert.False(true, "Should be an expression of length 3")


  // [<Fact>]
  // let ``set number variable`` () =
  //   let varName = "real"
  //   let lb = -1.0
  //   let ub = 2.0
  //   let mutable v = Variable.Real(varName, lb, ub)

  //   let newValue = 1.0
  //   v.Data.Number.toFloat |> should equal 0.0
  //   v <- v |> Variable.Set newValue
  //   v.Data.Number.toFloat |> should equal newValue


  // [<Fact>]
  // let ``set number variable throws error if out of bounds`` () =
  //   let varName = "real"
  //   let lb = -1.0
  //   let ub = 2.0
  //   let mutable v = Variable.Real(varName, lb, ub)

  //   let outOfBoundsValue = 9.0
  //   shouldFail (fun () -> Variable.Set outOfBoundsValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  // [<Fact>]
  // let ``set boolean variable throws error if set to number`` () =
  //   let mutable v = Variable.Bool "bool"

  //   let invalidValue = 9.0
  //   shouldFail (fun () -> Variable.Set invalidValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  // [<Fact>]
  // let ``set number variable throws error if set to boolean`` () =
  //   let mutable v = Variable.Integer("num", 0, 1)

  //   let invalidValue = true
  //   shouldFail (fun () -> Variable.Set invalidValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

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

