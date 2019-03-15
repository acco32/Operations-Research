namespace Operations.Research.Test

module ``Google Solver`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google


  [<Fact>]
  let ``basic linear program``() =
    true |> should be True