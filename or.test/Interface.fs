namespace Operations.Research.Test

module ``Basic Interface`` =

  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.LinearProgramming

  [<Fact>]
  let ``Create Boolean Variable with default values``() =
    let varName = "bool"
    let v = Variable.Boolean varName
    v.LowerBound |> should equal 0.0
    v.UpperBound |> should equal 1.0
    v.State |> should equal false
    v.Value |> should equal 0.0
    v.Name |> should equal varName

  [<Fact>]
  let ``Create Number Variable with default values``() =
    let varName = "real"
    let lb = -1.0
    let ub = 2.0
    let v = Variable.Number varName lb ub
    v.LowerBound |> should equal lb
    v.UpperBound |> should equal ub
    v.Value |> should equal 0.0
    v.Name |> should equal varName

  [<Fact>]
  let ``Create Boolean Operand`` () =
    let x = Variable.Boolean ""
    // let res =  2.0 * x
    let result = Operand(2.0, x)
    // result.Value |> should equal 0.0
    // x.True() = 1.0

    // let sdf2 = -2.0 * x
    // let sdf3 = -5 * y
    1 |> should equal 1


    // let expr = [] + 3*x + 1*z + -2*y + 6*x
    // let con1 = expr <== 6.0
    // let con2 = expr >== 6.0



