namespace Operations.Research.Test

open System
open TestTracks
open Operations.Research.Types
open Operations.Research.Models
open Operations.Research.Solvers.Google.Constraint

module GoogleSolverConstraint =

  let tests =
    suite
      "Google Solver - Constraint"
      [

        (*
            Rabbits & Pheasants.

            Find the number of rabbits (r) and pheasants (p) given that there are
            twenty animals total and fifty-six legs total. Rabbits have four legs,
            pheasants have two.

            r + p   = 20
            4r + 2p = 56
        *)
        test "Rabbits and Pheasants feasibility" (fun () ->
          let r = Variable.integer "r" 0 100
          let p = Variable.integer "p" 0 100

          let mdl =
            Model.empty
            |> DecisionVars [ r; p ]
            |> Constraints [ r + p === 20; 4 * r + 2 * p === 56 ]

          let sol = Solve mdl

          assertNotEqual Infeasible sol.Status "model should be feasible"
          |> combine (assertEqual 8.0 sol.Values.["r"] "rabbits should be 8")
          |> combine (assertEqual 12.0 sol.Values.["p"] "pheasants should be 12"))

        (*
            Sanity check that NotEqual constraints route through CP-SAT correctly,
            with a generous time limit. CP-SAT is the only solver in this library
            that supports the =/= operator.
        *)
        test "NotEqual constraint with 10 second time limit" (fun () ->
          let x = Variable.integer "x" 0 2
          let y = Variable.integer "y" 0 2
          let z = Variable.integer "z" 0 2

          let mdl =
            Model.empty |> DecisionVars [ x; y; z ] |> Constraints [ x + -1 * y =/= 0 ]

          let opts =
            { SolverOptions.Default with
                TimeLimit = 10 }

          let sol = SolveWithCustomOptions mdl opts

          assertNotEqual Infeasible sol.Status "model should be feasible"
          |> combine (assertEqual 1.0 sol.Values.["x"] "x should be 1")
          |> combine (assertEqual 0.0 sol.Values.["y"] "y should be 0")
          |> combine (assertEqual 0.0 sol.Values.["z"] "z should be 0")) ]
