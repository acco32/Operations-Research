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
    v.Value |> should equal 0.0
    v.Name |> should equal varName

  [<Fact>]
  let ``create number variable with default values``() =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let v = Variable.Num varName lb ub
    // v.LowerBound |> should equal lb
    // v.UpperBound |> should equal ub
    v.Value |> should equal 0.0
    v.Name |> should equal varName

  [<Fact>]
  let ``create boolean operand`` () =
    let x = Variable.Bool "a"
    let result = 1.0*x
    result |> should be instanceOfType<Operand>

  [<Fact>]
  let ``create number operand`` () =
    let x = Variable.Num "a" 0. 1.
    let result = 1.0*x
    result |> should be instanceOfType<Operand>

  [<Fact>]
  let ``create mixed 2-operand expression`` () =
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
  let ``create 3-operand expression`` () =
    let x = Variable.Num "a" 0. 1.
    let y = Variable.Num "b" 0. 1.
    let z = Variable.Num "c" 0. 1.

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
    let mutable v = Variable.Num varName lb ub

    let newValue = 1.0
    v.Value |> should equal 0.0
    v <- v |> Variable.Set newValue
    v.Value |> should equal newValue


  [<Fact>]
  let ``set number variable throws error if out of bounds`` () =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let mutable v = Variable.Num varName lb ub

    let outOfBoundsValue = 9.0
    shouldFail (fun () -> Variable.Set outOfBoundsValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set boolean variable throws error if set to number`` () =
    let mutable v = Variable.Bool "bool"

    let invalidValue = 9.0
    shouldFail (fun () -> Variable.Set invalidValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``set number variable throws error if set to boolean`` () =
    let mutable v = Variable.Num "num" 0. 1.

    let invalidValue = true
    shouldFail (fun () -> Variable.Set invalidValue v |> should throw typeof<System.ArgumentOutOfRangeException> )

  [<Fact>]
  let ``create default solver options should have default values of none`` () =
    let so = SolverParams.Default
    so.Variables |> should haveLength 0
    so.Objective |> should equal None
    so.Constraints |> should haveLength 0

  [<Fact>]
  let ``create constraint with less than or equal operator``()=
    let x = Variable.Num "x" 0. 1.
    let c = 1.0*x <== 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with greater than or equal operator``()=
    let x = Variable.Num "x" 0. 1.
    let c = 1.0*x >== 2.0
    c |> should be instanceOfType<Constraint>

  [<Fact>]
  let ``create constraint with equal operator``()=
    let x = Variable.Num "x" 0. 1.
    let c = 1.0*x == 2.0
    c |> should be instanceOfType<Constraint>



// https://github.com/google/or-tools/blob/stable/examples/contrib/fsProgram.fs
// https://github.com/google/or-tools/blob/stable/ortools/dotnet/Google.OrTools.FSharp/OrTools.fs
// https://github.com/google/or-tools/blob/stable/examples/contrib/fsequality.fs
// https://github.com/google/or-tools/blob/stable/examples/contrib/fsequality-inequality.fs

