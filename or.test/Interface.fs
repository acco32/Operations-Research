namespace Operations.Research.Test

module ``Basic Interface`` =

  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  // open Operations.Research.LinearProgramming

  let internal ops lst : (Operand list) =
    match lst with
    | Expression(l) -> l


  [<Fact>]
  let ``Create Boolean Variable with default values``() =
    let varName = "bool"
    let v = Variable.Bool varName
    v.BoolData().Value |> should equal false
    v.BoolData().Name |> should equal varName

  [<Fact>]
  let ``Create Number Variable with default values``() =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let v = Variable.Num varName lb ub
    v.NumberData().LowerBound |> should equal lb
    v.NumberData().UpperBound |> should equal ub
    v.NumberData().Value |> should equal 0.0
    v.NumberData().Name |> should equal varName

  [<Fact>]
  let ``Create Boolean Operand`` () =
    let x = Variable.Bool "a"
    let result = 1.0*x
    result |> should be instanceOfType<Operand>

  [<Fact>]
  let ``Create Number Operand`` () =
    let x = Variable.Num "a" 0. 1.
    let result = 1.0*x
    result |> should be instanceOfType<Operand>

  [<Fact>]
  let ``Create Mixed 2-Operand Expression`` () =
    let x = Variable.Num "a" 0. 1.
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
  let ``Create 3-Operand Expression`` () =
    let x = Variable.Num "a" 0. 1.
    let y = Variable.Num "b" 0. 1.
    let z = Variable.Num "c" 0. 1.

    let op1 = 1.0*x
    let op2 = 1.0*y
    let op3 = 1.0*z

    let result = op1 + op2 + op3
    result |> should be instanceOfType<Operand>

    let operands = ops result
    operands |> List.length |> should equal 3
    operands |> List.contains op1 |> should be True
    operands |> List.contains op2 |> should be True
    operands |> List.contains op3 |> should be True




