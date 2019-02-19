namespace Operations.Research.Test

module ``Basic Interface`` =

  open Xunit
  open FsUnit.Xunit
  open Operations.Research.LinearProgramming


  [<Fact>]
  let ``Initial Test`` () =
    let x = BooleanVariable "choice A"
    let y = RealVariable "choice B" 0.0 2.0
    let z = IntegerVariable "choice C" 0 5

    let sdf1 = 2 * x
    let sdf2 = -2.0 * x
    let sdf3 = -5 * y


    let expr = [] + 3*x + 1*z + -2*y + 6*x
    let con1 = expr <== 6.0
    let con2 = expr >== 6.0

    1 |> should equal 1