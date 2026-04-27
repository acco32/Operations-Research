namespace Operations.Research.Test

open System
open TestTracks
open Operations.Research.Types
open Operations.Research.Models
open Operations.Research.Solvers.Google.Linear

module GoogleSolverLinear =

    let private throws (f: unit -> 'a) : bool =
        try f () |> ignore; false with _ -> true

    let tests = suite "Google Solver - Linear" [

        test "basic linear program" (fun () ->
            let x = Variable.real "x" 0.0 1.0
            let y = Variable.real "y" 0.0 2.0
            let mdl =
                Model.empty
                |> DecisionVars [x; y]
                |> Goal Maximize
                |> Objective (1.0*x + 1.0*y)
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertEqual (Some 3.0) sol.Objective "objective should be 3.0")
        )

        test "linear program with integer variables and constant in objective" (fun () ->
            let x = Variable.integer "x" 0 1
            let y = Variable.integer "y" 0 2
            let mdl =
                Model.empty
                |> DecisionVars [x; y]
                |> Goal Maximize
                |> Objective (1*x + 1*y + 77)
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertEqual (Some 80.0) sol.Objective "objective should be 80.0")
        )

        test "boolean variable maximize" (fun () ->
            let x = Variable.boolean "x"
            let mdl =
                Model.empty
                |> DecisionVars [x]
                |> Goal Maximize
                |> Objective (1.0*x + 2.0)
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertEqual (Some 3.0) sol.Objective "objective should be 3.0")
        )

        test "boolean variable minimize" (fun () ->
            let x = Variable.boolean "x"
            let mdl =
                Model.empty
                |> DecisionVars [x]
                |> Goal Minimize
                |> Objective (1.0*x + 2.0)
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertEqual (Some 2.0) sol.Objective "objective should be 2.0")
        )

        test "linear program in matrix form" (fun () ->
            let x = Variable.real "x" 0.0 Double.PositiveInfinity
            let y = Variable.realFree "y"
            let m  = [[2.0; 1.0]; [1.0; 2.0]]
            let lb = [0.0; 0.0]
            let ub = [104.0; 76.0]
            let mdl =
                Model.empty
                |> DecisionVars [x; y]
                |> Goal Maximize
                |> Objective (6.0*x + 11.0*y)
                |> Matrix m lb ub
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertInDelta 44.0 sol.Values.["x"] 0.001 "x should be 44")
            |> combine (assertInDelta 16.0 sol.Values.["y"] 0.001 "y should be 16")
            |> combine (assertEqual (Some 440.0) sol.Objective "objective should be 440.0")
        )

        test "matrix form with equality constraints" (fun () ->
            let x  = Variable.real "x"  0.0 Double.PositiveInfinity
            let y  = Variable.real "y"  0.0 Double.PositiveInfinity
            let s1 = Variable.real "s1" 0.0 Double.PositiveInfinity
            let s2 = Variable.real "s2" 0.0 Double.PositiveInfinity
            let s3 = Variable.real "s3" 0.0 Double.PositiveInfinity
            let m = [
                [3.0; 1.0; -1.0;  0.0;  0.0]
                [4.0; 3.0;  0.0; -1.0;  0.0]
                [1.0; 2.0;  0.0;  0.0; -1.0]
            ]
            let eq = [3.0; 6.0; 2.0]
            let mdl =
                Model.empty
                |> DecisionVars [x; y; s1; s2; s3]
                |> Goal Minimize
                |> Objective (2.0*x + 1.0*y)
                |> MatrixEq m eq
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertInDelta 2.4 sol.Objective.Value 0.001 "objective should be 2.4")
            |> combine (assertInDelta 0.6 sol.Values.["x"]  0.001 "x should be 0.6")
            |> combine (assertInDelta 1.2 sol.Values.["y"]  0.001 "y should be 1.2")
            |> combine (assertInDelta 0.0 sol.Values.["s1"] 0.001 "s1 should be 0.0")
            |> combine (assertInDelta 0.0 sol.Values.["s2"] 0.001 "s2 should be 0.0")
            |> combine (assertInDelta 1.0 sol.Values.["s3"] 0.001 "s3 should be 1.0")
        )

        test "infeasible constraints return Infeasible status" (fun () ->
            let x = Variable.real "x" 0.0 Double.PositiveInfinity
            let y = Variable.real "y" 0.0 Double.PositiveInfinity
            let mdl =
                Model.empty
                |> DecisionVars [x; y]
                |> Goal Minimize
                |> Objective (-2.0*x + 3.0*y)
                |> Constraint (-1.0*x + 2.0*y <== 2.0)
                |> Constraint (2.0*x  + -1.0*y <== 3.0)
                |> Constraint (1.0*y >== 4.0)
            let sol = Solve mdl
            assertEqual Infeasible sol.Status "status should be Infeasible"
        )

        test "integer program with CBC strategy" (fun () ->
            let x = Variable.integerFree "x"
            let y = Variable.integerFree "y"
            let mdl =
                Model.empty
                |> DecisionVars [x; y]
                |> Goal Maximize
                |> Objective (1*x + 5*y)
                |> Constraints [
                    x + y <== 8
                    (-1*x) + 3*y <== 0
                ]
            let opts = { SolverOptions.Default with Strategy = IntegerSolverStrategy.CBC }
            let sol = SolveWithCustomOptions mdl opts
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertEqual (Some 16.0) sol.Objective "objective should be 16.0")
            |> combine (assertEqual 6.0 sol.Values.["x"] "x should be 6")
            |> combine (assertEqual 2.0 sol.Values.["y"] "y should be 2")
        )

        test "linear program with constant in objective function" (fun () ->
            let x = Variable.real "x" 0.0 Double.PositiveInfinity
            let y = Variable.real "y" 0.0 Double.PositiveInfinity
            let mdl =
                Model.empty
                |> DecisionVars [x; y]
                |> Goal Maximize
                |> Objective (6.0*x + 2.0*y + 77.0)
                |> Constraints [
                    3.0*x + 1.0*y <== 48.0
                    3.0*x + 4.0*y <== 120.0
                    3.0*x + 1.0*y >== 36.0
                ]
            let sol = SolveWithCustomOptions mdl SolverOptions.Default
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertInDelta 173.0 sol.Objective.Value 0.001 "objective should be 173.0")
            |> combine (assertInDelta 16.0 sol.Values.["x"] 0.001 "x should be 16")
            |> combine (assertInDelta 0.0 sol.Values.["y"] 0.001 "y should be 0")
        )

        test "maximum flow problem in matrix form" (fun () ->
            let arc01 = Variable.real "arc01" 0.0 3.0
            let arc02 = Variable.real "arc02" 0.0 2.0
            let arc03 = Variable.real "arc03" 0.0 2.0
            let arc14 = Variable.real "arc14" 0.0 5.0
            let arc15 = Variable.real "arc15" 0.0 1.0
            let arc24 = Variable.real "arc24" 0.0 1.0
            let arc25 = Variable.real "arc25" 0.0 3.0
            let arc26 = Variable.real "arc26" 0.0 1.0
            let arc35 = Variable.real "arc35" 0.0 1.0
            let arc47 = Variable.real "arc47" 0.0 4.0
            let arc57 = Variable.real "arc57" 0.0 2.0
            let arc67 = Variable.real "arc67" 0.0 4.0
            let m = [
                [1.0; 0.0; 0.0; -1.0; -1.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0]
                [0.0; 1.0; 0.0;  0.0;  0.0; -1.0; -1.0; -1.0;  0.0;  0.0;  0.0;  0.0]
                [0.0; 0.0; 1.0;  0.0;  0.0;  0.0;  0.0;  0.0; -1.0;  0.0;  0.0;  0.0]
                [0.0; 0.0; 0.0;  1.0;  0.0;  1.0;  0.0;  0.0;  0.0; -1.0;  0.0;  0.0]
                [0.0; 0.0; 0.0;  0.0;  1.0;  0.0;  1.0;  0.0;  1.0;  0.0; -1.0;  0.0]
                [0.0; 0.0; 0.0;  0.0;  0.0;  0.0;  0.0;  1.0;  0.0;  0.0;  0.0; -1.0]
            ]
            let b = List.replicate 6 0.0
            let mdl =
                Model.empty
                |> DecisionVars [arc01; arc02; arc03; arc14; arc15; arc24; arc25; arc26; arc35; arc47; arc57; arc67]
                |> Goal Maximize
                |> Objective (1.0*arc01 + 1.0*arc02 + 1.0*arc03)
                |> MatrixEq m b
            let sol = Solve mdl
            assertEqual Optimal sol.Status "solver should reach optimal"
            |> combine (assertInDelta 6.0 sol.Objective.Value 0.001 "max flow should be 6")
            |> combine (assertInDelta 3.0 sol.Values.["arc01"] 0.001 "arc01 should saturate at 3")
            |> combine (assertInDelta 2.0 sol.Values.["arc02"] 0.001 "arc02 should saturate at 2")
            |> combine (assertInDelta 1.0 sol.Values.["arc03"] 0.001 "arc03 should be 1")
            |> combine (assertInDelta 2.0 sol.Values.["arc14"] 0.001 "arc14 should be 2")
            |> combine (assertInDelta 1.0 sol.Values.["arc15"] 0.001 "arc15 should be 1")
            |> combine (assertInDelta 1.0 sol.Values.["arc24"] 0.001 "arc24 should be 1")
            |> combine (assertInDelta 0.0 sol.Values.["arc25"] 0.001 "arc25 should be 0")
            |> combine (assertInDelta 1.0 sol.Values.["arc26"] 0.001 "arc26 should be 1")
            |> combine (assertInDelta 1.0 sol.Values.["arc35"] 0.001 "arc35 should be 1")
            |> combine (assertInDelta 3.0 sol.Values.["arc47"] 0.001 "arc47 should be 3")
            |> combine (assertInDelta 2.0 sol.Values.["arc57"] 0.001 "arc57 should be 2")
            |> combine (assertInDelta 1.0 sol.Values.["arc67"] 0.001 "arc67 should be 1")
        )

        test "linear solver rejects disjunctive (NotEqual) constraint" (fun () ->
            let x = Variable.integer "x" -6 6
            let y = Variable.integer "y" -6 6
            let go () =
                let mdl =
                    Model.empty
                    |> DecisionVars [x; y]
                    |> Goal Maximize
                    |> Objective (-0.5*x + 1*y + -2)
                    |> Constraints [ 1*x =/= 2 ]
                let opts = { SolverOptions.Default with Strategy = IntegerSolverStrategy.CBC }
                SolveWithCustomOptions mdl opts
            assertTrue (throws go) "linear solver should throw on NotEqual"
        )
    ]