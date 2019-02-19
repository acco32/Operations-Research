namespace Operations.Research.Test

module ``Basic Interface`` =

  open Xunit
  open FsUnit.Xunit


  [<Fact>]
  let ``Initial Test`` () =
      1 |> should equal 1