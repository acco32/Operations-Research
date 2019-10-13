namespace Operations.Research.Test

module ``Basic Types`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models

  let internal ops lst : (Operand list) =
    match lst with
    | Expression(l) -> l


  [<Fact>]
  let ``create boolean variable with default values``() =
    let varName = "bool"
    let v = Variable.Bool varName
    v.Data.Selected |> should be False
    v.Name |> should equal varName

  [<Fact>]
  let ``create number variable with default values``() =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0

    let v = Variable.Real(varName, lb, ub)

    v.LowerBound.toFloat |> should equal (lb)
    v.UpperBound.toFloat |> should equal (ub)

    v.Data.Number.toFloat |> should equal (0.0)
    v.Name |> should equal varName

  [<Fact>]
  let ``create boolean operand`` () =
    let x = Variable.Bool "a"
    let result = 1*x
    result |> should be instanceOfType<Operand>

  [<Fact>]
  let ``create number (integer) operand`` () =
    let x = Variable.Integer("a", 0, 1)
    let result = 1*x
    result |> should be instanceOfType<Operand>

  let ``create number (float) operand`` () =
    let x = Variable.Real("a", 0., 1.)
    let result = 1*x
    result |> should be instanceOfType<Operand>

  [<Fact>]
  let ``able to create mixed 2-operand expression`` () =
    let x = Variable.Integer "a"
    let y = Variable.Bool "b"

    let op1 = 1.0*x
    let op2 = 1.0*y

    let result = op1 + op2
    result |> should be instanceOfType<Operand>

    let operands = ops result
    operands |> List.length |> should equal 2
    operands |> List.contains op1 |> should be True
    operands |> List.contains op2 |> should be True


  [<Fact>]
  let ``able to create mixed 3-operand expression`` () =
    let x = Variable.Integer("a", 0, 1)
    let y = Variable.Real("b", 0., 1.)
    let z = Variable.Bool("c")

    let op1 = 1.0*x
    let op2 = 1.0*y
    let op3 = 1.0*z

    let result = op1 + op2 + op3
    result |> should be instanceOfType<Operand>

    let operands = ops result
    operands |> should haveLength 3
    operands |> List.contains op1 |> should be True
    operands |> List.contains op2 |> should be True
    operands |> List.contains op3 |> should be True

  [<Fact>]
  let ``set number variable`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let mutable v = Variable.Real(varName, lb, ub)

    let newValue = 1.0
    v.Data.Number.toFloat |> should equal 0.0
    v <- v |> Variable.Set newValue
    v.Data.Number.toFloat |> should equal newValue


  [<Fact>]
  let ``set number variable throws error if out of bounds`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let mutable v = Variable.Real(varName, lb, ub)

    let outOfBoundsValue = 9.0
    shouldFail (fun () -> Variable.Set outOfBoundsValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set boolean variable throws error if set to number`` () =
    let mutable v = Variable.Bool "bool"

    let invalidValue = 9.0
    shouldFail (fun () -> Variable.Set invalidValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set number variable throws error if set to boolean`` () =
    let mutable v = Variable.Integer("num", 0, 1)

    let invalidValue = true
    shouldFail (fun () -> Variable.Set invalidValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``create default solver options should have default values of none`` () =
    let mdl = Model.Default
    mdl.Variables |> should haveLength 0
    mdl.Objective |> should equal None
    mdl.Constraints |> should haveLength 0

  [<Fact>]
  let ``create constraint with less than or equal operator``()=
    let x = Variable.Real("x", 0., 1.)
    let c = 1.0*x <== 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with greater than or equal operator``()=
    let x = Variable.Real("x", 0., 1.)
    let c = 1.0*x >== 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with equal operator``()=
    let x = Variable.Real("x", 0., 1.)
    let c = 1.0*x === 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with not equal operator throws error if boundary value is not an integer``()=
    let x = Variable.Real("x", 0., 1.)
    shouldFail (fun () -> 1*x =/= 2 |> should throw typeof<ArgumentException> )


