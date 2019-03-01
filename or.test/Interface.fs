namespace Operations.Research.Test

module ``Basic Interface`` =

  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.LinearProgramming

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


  // let ``Create Boolean Operand`` () =
  //   let x = Variable.Bool "a"
  //   let y = Variable.Bool "b"

  //   // let result = 1.0*x + 1.0*y
    // result.Value |> should equal 0.0
    // x.True() = 1.0


