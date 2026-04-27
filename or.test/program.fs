namespace Operations.Research.Test

open TestTracks

module Program =

  [<EntryPoint>]
  let main argv =
    let models = [ Models.tests ]
    let examples = [ Examples.tests ]

    let solvers =
      [ GoogleSolverConstraint.tests
        GoogleSolverLinear.tests
        GoogleSolverRouting.tests ]

    let all = models @ solvers @ examples

    match argv with
    | [| "--models" |] -> parseTestArgs [||] models
    | [| "--solvers" |] -> parseTestArgs [||] solvers
    | [| "--examples" |] -> parseTestArgs [||] examples
    | _ -> parseTestArgs argv all
