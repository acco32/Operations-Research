namespace Operations.Research.Test

module ``Basic Types`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types


  [<Fact>]
  let ``create boolean variable with default values``() =
    let varName = "bool"
    let v = Variable.boolean varName

    v.Value.toInt |> should equal 0
    v.Value.toFloat |> should equal 0.0
    v.Name |> should equal varName
    Variable.state(v) |> should be False

  [<Fact>]
  let ``create number variable with default values``() =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0

    let v = Variable.real varName lb ub

    v.Bounds.Lower.toFloat |> should equal lb
    v.Bounds.Upper.toFloat |> should equal ub

    v.Value.toFloat |> should equal Double.NegativeInfinity
    v.Name |> should equal varName

  [<Fact>]
  let ``create boolean operand`` () =
    let x = Variable.boolean "a" |> toExpression
    let result = 1*x
    result |> should be instanceOfType<Expression>

  [<Fact>]
  let ``create number (integer) operand`` () =
    let x = Variable.integer "a" 0 1 |> toExpression
    let result = 1*x
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 1


  [<Fact>]
  let ``create number--float--operand`` () =
    let x = Variable.real "a" 0. 1. |> toExpression
    let result = 1*x
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 1


  [<Fact>]
  let ``add two--integer--variables and create an expression``() =
    let x = Variable.integer "a" Int32.MinValue Int32.MaxValue |> toExpression
    let y = Variable.integerDefault "b" |> toExpression

    let result = x + y
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 2


  [<Fact>]
  let ``add number variable--integer--and constant should create an expression``() =
    let x = Variable.integerDefault "a" |> toExpression

    let result = x + 77
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 2


  [<Fact>]
  let ``add number variable--real--and constant should create an expression``() =
    let x = Variable.realDefault "a" |> toExpression

    let result = x + 77.0
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 2


  [<Fact>]
  let ``add number variable (real) and coefficient-variable (real) should create an expression``() =
    let x = Variable.realDefault "a" |> toExpression
    let y = Variable.realDefault "b" |> toExpression

    let result = x + 2*y
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 2


  [<Fact>]
  let ``create a multi-variable expression from integer variables`` () =
    let x = Variable.integer "a" 0 10 |> toExpression
    let y = Variable.integer "b" 0 10 |> toExpression
    let z = Variable.integer "c" 0 10 |> toExpression

    let result = x + 2*y + 5*z + 80
    result |> should be instanceOfType<Expression>
    result.Terms |> should haveLength 4


  [<Fact>]
  let ``set number variable`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let mutable v = Variable.real varName lb ub

    let newValue = 1.0
    v.Value.toFloat |> should equal Double.NegativeInfinity
    v <- v |> Variable.set newValue
    v.Value.toFloat |> should equal newValue


  [<Fact>]
  let ``set number variable throws error if out of bounds`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let v = Variable.real varName lb ub

    let outOfBoundsValue = 9.0
    shouldFail (fun () -> v |> Variable.set outOfBoundsValue |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set boolean variable throws error if set to number`` () =
    let v = Variable.boolean "bool"

    let invalidValue = 9.0
    shouldFail (fun () -> v |> Variable.set invalidValue |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set number variable throws error if set to boolean`` () =
    let v = Variable.integer "num" 0 1

    let invalidValue = true
    shouldFail (fun () -> v |> Variable.set invalidValue |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``evaluate an expression with with all operand types``() =

    let value = 5
    let x = Variable.integerDefault "a" |> Variable.set value |> toExpression
    let y = Variable.integerDefault "b" |> Variable.set value |> toExpression

    let expression = x + 2*y + 10
    let result = eval(expression)

    result |> should be instanceOfType<Number>
    result.toInt |> should equal 25

