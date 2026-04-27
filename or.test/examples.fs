namespace Operations.Research.Test

open System
open TestTracks
open Operations.Research.Types
open Operations.Research.Models
open Operations.Research.Solvers.Google.Linear
open Google.OrTools.Graph
open Google.OrTools.Algorithms

module Examples =

    let tests = suite "Examples" [

        (*
            Linear program in non-standard form.

            Minimize:    -3*x0 + x1 + x2
            subject to:  x0 - 2*x1 + x2  <= 11
                         -4*x0 + x1 + 2*x2 >= 3
                         -2*x0 - x2        = -1
            where:       xi >= 0
        *)
        test "linear program non-standard form" (fun () ->
            let x0 = Variable.real "x0" 0.0 Double.PositiveInfinity
            let x1 = Variable.real "x1" 0.0 Double.PositiveInfinity
            let x2 = Variable.real "x2" 0.0 Double.PositiveInfinity
            let mdl =
                Model.empty
                |> DecisionVars [x0; x1; x2]
                |> Goal Minimize
                |> Objective (-3*x0 + 1*x1 + 1*x2)
                |> Constraints [
                    1*x0 + (-2*x1) + 1*x2  <== 11
                    (-4*x0) + 1*x1 + 2*x2  >== 3
                    (-2*x0) + (-1*x2)      === -1
                ]
            let sol = SolveWithCustomOptions mdl SolverOptions.Default
            assertEqual Optimal sol.Status "should reach optimal"
            |> combine (assertInDelta 2.0 sol.Objective.Value 0.001 "objective should be 2.0")
            |> combine (assertInDelta 0.0 sol.Values.["x0"] 0.001 "x0 should be 0")
            |> combine (assertInDelta 1.0 sol.Values.["x1"] 0.001 "x1 should be 1")
            |> combine (assertInDelta 1.0 sol.Values.["x2"] 0.001 "x2 should be 1")
        )

        (*
            Same problem in matrix form, with explicit slack variables (s0, s1) added
            so we can inspect them in the solution. The inequality constraints become
            equalities once slacks are introduced.
        *)
        test "linear program non-standard form with matrices" (fun () ->
            let x0 = Variable.real "x0" 0.0 Double.PositiveInfinity
            let x1 = Variable.real "x1" 0.0 Double.PositiveInfinity
            let x2 = Variable.real "x2" 0.0 Double.PositiveInfinity
            let s0 = Variable.real "s0" 0.0 Double.PositiveInfinity
            let s1 = Variable.real "s1" 0.0 Double.PositiveInfinity
            let m = [
                [ 1.0; -2.0;  1.0; 1.0;  0.0]
                [-4.0;  1.0;  2.0; 0.0; -1.0]
                [-2.0;  0.0; -1.0; 0.0;  0.0]
            ]
            let eq = [11.0; 3.0; -1.0]
            let mdl =
                Model.empty
                |> DecisionVars [x0; x1; x2; s0; s1]
                |> Goal Minimize
                |> Objective (-3*x0 + 1*x1 + 1*x2)
                |> MatrixEq m eq
            let sol = Solve mdl
            assertEqual Optimal sol.Status "should reach optimal"
            |> combine (assertInDelta 2.0  sol.Objective.Value 0.001 "objective")
            |> combine (assertInDelta 0.0  sol.Values.["x0"]   0.001 "x0")
            |> combine (assertInDelta 1.0  sol.Values.["x1"]   0.001 "x1")
            |> combine (assertInDelta 1.0  sol.Values.["x2"]   0.001 "x2")
            |> combine (assertInDelta 12.0 sol.Values.["s0"]   0.001 "s0")
            |> combine (assertInDelta 0.0  sol.Values.["s1"]   0.001 "s1")
        )

        (*
            Stigler's diet (variant by Hakan Kjellerstrand).

            Pick whole units of products A, B, C, D to meet daily nutritional minimums
            at minimum cost. Each product carries calories, salt, sugar, fat, and a
            price per unit. Stay healthy as cheaply as possible.
        *)
        test "Stigler's Diet" (fun () ->
            let A = Variable.integer "A" 0 100
            let B = Variable.integer "B" 0 100
            let C = Variable.integer "C" 0 100
            let D = Variable.integer "D" 0 100
            let mdl =
                Model.empty
                |> DecisionVars [A; B; C; D]
                |> Goal Minimize
                |> Objective (50*A + 20*B + 30*C + 80*D)
                |> Constraints [
                    400*A + 200*B + 150*C + 500*D >== 500   // calories
                    3*A + 2*B                       >== 6     // salt
                    2*A + 2*B + 4*C + 4*D           >== 10    // sugar
                    2*A + 4*B + 1*C + 5*D           >== 8     // fat
                ]
            let sol = Solve mdl
            assertEqual Optimal sol.Status "should reach optimal"
            |> combine (assertInDelta 90.0 sol.Objective.Value 0.001 "min cost should be 90")
            |> combine (assertInDelta 0.0  sol.Values.["A"] 0.001 "A")
            |> combine (assertInDelta 3.0  sol.Values.["B"] 0.001 "B")
            |> combine (assertInDelta 1.0  sol.Values.["C"] 0.001 "C")
            |> combine (assertInDelta 0.0  sol.Values.["D"] 0.001 "D")
        )

        (*
            Maximum flow as a linear program.

            Six interior nodes plus a pseudo-source (0) and pseudo-sink (7). Maximize
            total flow leaving the source, subject to flow conservation at every
            interior node and capacity limits on each arc.

            Maximize:    x01 + x02 + x03
            subject to:  flow conservation at nodes 1..6
            where:       0 <= x_ij <= capacity_ij
        *)
        test "Maximum Flow - Linear Program" (fun () ->
            let x01 = Variable.real "arc_0->1" 0.0 3.0
            let x02 = Variable.real "arc_0->2" 0.0 2.0
            let x03 = Variable.real "arc_0->3" 0.0 2.0
            let x14 = Variable.real "arc_1->4" 0.0 5.0
            let x15 = Variable.real "arc_1->5" 0.0 1.0
            let x24 = Variable.real "arc_2->4" 0.0 1.0
            let x25 = Variable.real "arc_2->5" 0.0 3.0
            let x26 = Variable.real "arc_2->6" 0.0 1.0
            let x35 = Variable.real "arc_3->5" 0.0 1.0
            let x47 = Variable.real "arc_4->7" 0.0 4.0
            let x57 = Variable.real "arc_5->7" 0.0 2.0
            let x67 = Variable.real "arc_6->7" 0.0 4.0
            let mdl =
                Model.empty
                |> DecisionVars [x01; x02; x03; x14; x15; x24; x25; x26; x35; x47; x57; x67]
                |> Goal Maximize
                |> Objective (1.0*x01 + 1.0*x02 + 1.0*x03)
                |> Constraints [
                    1.0*x01 + -1*x14 + -1*x15            === 0
                    1.0*x02 + -1*x24 + -1*x25 + -1*x26   === 0
                    1.0*x03 + -1*x35                     === 0
                    1.0*x14 + 1.0*x24 + -1*x47           === 0
                    1.0*x15 + 1.0*x25 + 1.0*x35 + -1*x57 === 0
                    1.0*x26 + -1*x67                     === 0
                ]
            let sol = Solve mdl
            assertEqual Optimal sol.Status "should reach optimal"
            |> combine (assertInDelta 6.0 sol.Objective.Value 0.001 "max flow")
            |> combine (assertInDelta 3.0 sol.Values.["arc_0->1"] 0.001 "arc_0->1")
            |> combine (assertInDelta 2.0 sol.Values.["arc_0->2"] 0.001 "arc_0->2")
            |> combine (assertInDelta 1.0 sol.Values.["arc_0->3"] 0.001 "arc_0->3")
            |> combine (assertInDelta 2.0 sol.Values.["arc_1->4"] 0.001 "arc_1->4")
            |> combine (assertInDelta 1.0 sol.Values.["arc_1->5"] 0.001 "arc_1->5")
            |> combine (assertInDelta 1.0 sol.Values.["arc_2->4"] 0.001 "arc_2->4")
            |> combine (assertInDelta 0.0 sol.Values.["arc_2->5"] 0.001 "arc_2->5")
            |> combine (assertInDelta 1.0 sol.Values.["arc_2->6"] 0.001 "arc_2->6")
            |> combine (assertInDelta 1.0 sol.Values.["arc_3->5"] 0.001 "arc_3->5")
            |> combine (assertInDelta 3.0 sol.Values.["arc_4->7"] 0.001 "arc_4->7")
            |> combine (assertInDelta 2.0 sol.Values.["arc_5->7"] 0.001 "arc_5->7")
            |> combine (assertInDelta 1.0 sol.Values.["arc_6->7"] 0.001 "arc_6->7")
        )

        (*
            Same max-flow network, but each conservation equation is loosened from a
            strict equality to a tight numerical range. Useful when the LP solver's
            tolerance gets in the way of pure equality.
        *)
        test "Maximum Flow - Linear Program - Range Operator" (fun () ->
            let x01 = Variable.real "arc_0->1" 0.0 3.0
            let x02 = Variable.real "arc_0->2" 0.0 2.0
            let x03 = Variable.real "arc_0->3" 0.0 2.0
            let x14 = Variable.real "arc_1->4" 0.0 5.0
            let x15 = Variable.real "arc_1->5" 0.0 1.0
            let x24 = Variable.real "arc_2->4" 0.0 1.0
            let x25 = Variable.real "arc_2->5" 0.0 3.0
            let x26 = Variable.real "arc_2->6" 0.0 1.0
            let x35 = Variable.real "arc_3->5" 0.0 1.0
            let x47 = Variable.real "arc_4->7" 0.0 4.0
            let x57 = Variable.real "arc_5->7" 0.0 2.0
            let x67 = Variable.real "arc_6->7" 0.0 4.0
            let eps = (-0.0001, 0.0001)
            let mdl =
                Model.empty
                |> DecisionVars [x01; x02; x03; x14; x15; x24; x25; x26; x35; x47; x57; x67]
                |> Goal Maximize
                |> Objective (1.0*x01 + 1.0*x02 + 1.0*x03)
                |> Constraints [
                    (1.0*x01 + -1*x14 + -1*x15)            <-> eps
                    (1.0*x02 + -1*x24 + -1*x25 + -1*x26)   <-> eps
                    (1.0*x03 + -1*x35)                     <-> eps
                    (1.0*x14 + 1.0*x24 + -1*x47)           <-> eps
                    (1.0*x15 + 1.0*x25 + 1.0*x35 + -1*x57) <-> eps
                    (1.0*x26 + -1*x67)                     <-> eps
                ]
            let sol = Solve mdl
            assertEqual Optimal sol.Status "should reach optimal"
            |> combine (assertInDelta 6.0 sol.Objective.Value 0.001 "max flow")
        )

        (*
            Same problem solved with OR-Tools' specialized MaxFlow algorithm rather
            than as an LP. Faster and more direct, but bypasses our Model layer.
            Bundled here as a reference for users who want the dedicated solver.
        *)
        test "Maximum Flow - Google.Graph (direct OR-Tools)" (fun () ->
            let numArcs = 9
            let tails       = [ 0;  0;  0;  0;  1;  2;  3;  3;  4]
            let heads       = [ 1;  2;  3;  4;  3;  4;  4;  5;  5]
            let capacities  = [5L; 8L; 5L; 3L; 4L; 5L; 6L; 6L; 4L]
            let expectedFlows     = [4L; 4L; 2L; 0L; 4L; 4L; 0L; 6L; 4L]
            let expectedTotalFlow = 10L
            let maxFlow = new MaxFlow()
            for i in 0 .. (numArcs - 1) do
                let arc = maxFlow.AddArcWithCapacity(tails.[i], heads.[i], capacities.[i])
                if arc <> i then failwith "Internal error"
            let solveStatus = maxFlow.Solve(0, 5)
            let isOptimal = solveStatus = MaxFlow.Status.OPTIMAL
            let initial =
                if isOptimal then
                    assertEqual expectedTotalFlow (maxFlow.OptimalFlow()) "total flow"
                else
                    fail "MaxFlow did not reach OPTIMAL status"
            [0 .. numArcs - 1]
            |> List.fold
                (fun acc i ->
                    if isOptimal then
                        acc |> combine (assertEqual expectedFlows.[i] (maxFlow.Flow(i)) (sprintf "arc %d flow" i))
                    else acc)
                initial
        )

        (*
            Knapsack as a 0/1 mixed-integer program.

            Saturday grocery shopping: $80 budget, a list of items each with a price.
            Pick a subset that maximizes total selected value without exceeding
            budget. In this variant value equals weight, so picking an item costs
            its value.

            Maximize:    sum(value_i * x_i)
            subject to:  sum(weight_i * x_i) <= 80
            where:       x_i in {0, 1}
        *)
        test "Knapsack - 0/1 MIP" (fun () ->
            let costs = [9; 8; 2; 12; 4; 6; 10; 7; 4; 2; 11; 9; 4; 16; 7; 10; 2; 12; 8]
            let budget = 80
            let vars =
                costs
                |> List.mapi (fun i _ -> Variable.boolean (sprintf "item_%02d" (i + 1)))
            let terms =
                List.zip costs vars
                |> List.map (fun (c, v) -> c * v)
            let objective        = LinearExpression.sum terms
            let weightConstraint = LinearExpression.sum terms
            let mdl =
                Model.empty
                |> DecisionVars vars
                |> Goal Maximize
                |> Objective objective
                |> Constraint (weightConstraint <== budget)
            let opts = { SolverOptions.Default with Strategy = IntegerSolverStrategy.CBC }
            let sol = SolveWithCustomOptions mdl opts
            let totalCost =
                vars
                |> List.mapi (fun i v ->
                    if sol.Values.[v.Name] > 0.5 then costs.[i] else 0)
                |> List.sum
            assertEqual Optimal sol.Status "should reach optimal"
            |> combine (assertTrue (totalCost <= budget) "selected items should fit budget")
            |> combine (assertInDelta (float budget) sol.Objective.Value 0.001 "should saturate budget")
        )

        (*
            Same knapsack solved with OR-Tools' dedicated KnapsackSolver. Faster than
            a general MIP for this problem class, but only works for the specific
            shape (sum-of-weights bounded by capacities). Bundled as a reference.
        *)
        test "Knapsack - Google.Algorithms (direct OR-Tools)" (fun () ->
            let costs       = [9L; 8L; 2L; 12L; 4L; 6L; 10L; 7L; 4L; 2L; 11L; 9L; 4L; 16L; 7L; 10L; 2L; 12L; 8L]
            let weights     = costs    // value-equals-weight variant
            let capacities  = [80L]
            let totalItems  = costs.Length
            let weightMatrix = array2D [List.toSeq weights]
            let solver =
                new KnapsackSolver(
                    KnapsackSolver.SolverType.KNAPSACK_DYNAMIC_PROGRAMMING_SOLVER,
                    "ks")
            solver.Init(List.toArray costs, weightMatrix, List.toArray capacities)
            let totalProfit = solver.Solve()
            let selectedWeight =
                [0 .. totalItems - 1]
                |> List.filter (fun i -> solver.BestSolutionContains(i))
                |> List.sumBy (fun i -> weights.[i])
            assertEqual 80L totalProfit "total profit should saturate the 80-unit budget"
            |> combine (assertTrue (selectedWeight <= 80L) "selected items should fit capacity")
        )
    ]
