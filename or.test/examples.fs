namespace Operations.Research.Test

module ``Examples`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google.Linear

  open Google.OrTools.Graph


  [<Fact>]
  let ``linear program non-standard form``() =
    let x0 = Variable.real "x0" 0.0 Double.PositiveInfinity |> toExpression
    let x1 = Variable.real "x1" 0.0 Double.PositiveInfinity |> toExpression
    let x2 = Variable.real "x2" 0.0 Double.PositiveInfinity |> toExpression


    let mdl =
      Model.Default
      |> DecisionVars [x0; x1; x2]
      |> Goal Minimize
      |> Objective  (-3*x0 + x1 + x2)
      |> Constraints [
        x0 + (-2*x1) + x2 <== 11
        (-4*x0) + x1 + (2*x2) >== 3
        (-2*x0) +  (-1*x2) === -1
      ]

    let result = SolveWithCustomOptions mdl SolverOptions.Default

    match result with
    | Solution sol ->
        sol.Objective.toFloat |> should (equalWithin 0.001) 2.0
        sol.Variables.[x0.var().Name] |> should (equalWithin 0.001) 0.0
        sol.Variables.[x1.var().Name] |> should (equalWithin 0.001) 1.0
        sol.Variables.[x2.var().Name] |> should (equalWithin 0.001) 1.0
    | Error e ->
        printfn "%A" e


  [<Fact>]
  let ``linear program non-standard form with matrices``() =
    let x0 = Variable.real "x0" 0.0 Double.PositiveInfinity |> toExpression
    let x1 = Variable.real "x1" 0.0 Double.PositiveInfinity |> toExpression
    let x2 = Variable.real "x2" 0.0 Double.PositiveInfinity |> toExpression

    let s0 = Variable.real "s0" 0.0 Double.PositiveInfinity |> toExpression
    let s1 = Variable.real "s1" 0.0 Double.PositiveInfinity |> toExpression


    let m = [
      [1.0 ; -2.0 ; 1.0 ; 1.0 ; 0.0];
      [-4.0 ; 1.0 ; 2.0 ; 0.0 ; -1.0];
      [-2.0 ; 0.0 ; -1.0 ; 0.0 ; 0.0];
    ]

    let eq = [11.0; 3.0; -1.0]

    let mdl =
          Model.Default
          |> DecisionVars [x0; x1; x2; s0; s1]
          |> Goal Minimize
          |> Objective  (-3*x0 + x1 + x2)
          |> MatrixEq m eq

    let result = Solve mdl

    match result with
    | Solution sol ->
        sol.Objective.toFloat |> should (equalWithin 0.001) 2.0
        sol.Variables.[x0.var().Name] |> should (equalWithin 0.001) 0.0
        sol.Variables.[x1.var().Name] |> should (equalWithin 0.001) 1.0
        sol.Variables.[x2.var().Name] |> should (equalWithin 0.001) 1.0

        sol.Variables.[s0.var().Name] |> should (equalWithin 0.001) 12.0
        sol.Variables.[s1.var().Name] |> should (equalWithin 0.001) 0.0
    | Error e ->
        printfn "%A" e


  [<Fact>]
  let ``Stigler's Diet``() =

    // products
    let A = Variable.integer  "A" 0 100 |> toExpression
    let B = Variable.integer  "B" 0 100 |> toExpression
    let C = Variable.integer  "C" 0 100 |> toExpression
    let D = Variable.integer  "D" 0 100 |> toExpression


    let mdl =
      Model.Default
      |> DecisionVars [A; B; C; D]
      |> Goal Minimize
      |> Objective (50*A + 20*B + 30*C + 80*D)
      |> Constraints [
            400*A + 200*B + 150*C + 500*D >== 500;  // calories with minimum required intake
            3*A + 2*B  >== 6; // salt with minimum required intake
            2*A + 2*B + 4*C + 4*D >== 10; // sugar with minimum required intake
            2*A + 4*B + C + 5*D >== 8; // fat with minimum required intake
          ]

    let result = Solve mdl

    match result with
    | Solution sol ->
        sol.Objective.toInt |> should (equalWithin 0.001) 90.0
        sol.Variables.[A.var().Name] |> should (equalWithin 0.001) 0.0
        sol.Variables.[B.var().Name] |> should (equalWithin 0.001) 3.0
        sol.Variables.[C.var().Name] |> should (equalWithin 0.001) 1.0
        sol.Variables.[D.var().Name] |> should (equalWithin 0.001) 0.0

    | Error e ->
        printfn "%A" e


  [<Fact>]
  let ``Maximum Flow - Linear Program``() =

    let x01 = Variable.real "arc_0->1" 0 3 |> toExpression
    let x02 = Variable.real "arc_0->2" 0 2 |> toExpression
    let x03 = Variable.real "arc_0->3" 0 2 |> toExpression

    let x14 = Variable.real "arc_1->4" 0 5 |> toExpression
    let x15 = Variable.real "arc_1->5" 0 1 |> toExpression

    let x24 = Variable.real "arc_2->4" 0 1 |> toExpression
    let x25 = Variable.real "arc_2->5" 0 3 |> toExpression
    let x26 = Variable.real "arc_2->6" 0 1 |> toExpression

    let x35 = Variable.real "arc_3->5" 0 1 |> toExpression

    let x47 = Variable.real "arc_4->7" 0 4 |> toExpression

    let x57 = Variable.real "arc_5->7" 0 2 |> toExpression

    let x67 = Variable.real "arc_6->7" 0 4 |> toExpression

    let mdl =
      Model.Default
      |> DecisionVars [x01; x02; x03; x14; x15; x24; x25; x26; x35; x47; x57; x67]
      |> Goal Maximize
      |> Objective (x01 + x02 + x03)
      |> Constraints [
            x01 + -1*x14 + -1*x15 === 0 // node 1
            x02 + -1*x24 + -1*x25 + -1*x26 === 0 // node 2
            x03 + -1*x35 === 0 // node 3
            x14 + x24 + -1*x47 === 0 // node 4
            x15 + x25 + x35 + -1*x57 === 0 // node 5
            x26 + -1*x67 === 0 // node 6
          ]

    let result = Solve mdl

    match result with
    | Solution sol ->
        sol.Objective.toInt |> should (equalWithin 0.001) 6
        sol.Variables.[x01.var().Name] |> should (equalWithin 0.001) 3
        sol.Variables.[x02.var().Name] |> should (equalWithin 0.001) 2
        sol.Variables.[x03.var().Name] |> should (equalWithin 0.001) 1
        sol.Variables.[x14.var().Name] |> should (equalWithin 0.001) 3
        sol.Variables.[x15.var().Name] |> should (equalWithin 0.001) 0
        sol.Variables.[x24.var().Name] |> should (equalWithin 0.001) 1
        sol.Variables.[x25.var().Name] |> should (equalWithin 0.001) 0
        sol.Variables.[x26.var().Name] |> should (equalWithin 0.001) 1
        sol.Variables.[x35.var().Name] |> should (equalWithin 0.001) 1
        sol.Variables.[x47.var().Name] |> should (equalWithin 0.001) 4
        sol.Variables.[x57.var().Name] |> should (equalWithin 0.001) 1
        sol.Variables.[x67.var().Name] |> should (equalWithin 0.001) 1

    | Error e ->
        printfn "%A" e


  [<Fact>]
  let ``Maximum Flow - Google.Graph``() =

    let numNodes = 6;
    let numArcs = 9;
    let tails = [0; 0; 0; 0; 1; 2; 3; 3; 4]
    let heads = [1; 2; 3; 4; 3; 4; 4; 5; 5]
    let capacities = [5L; 8L; 5L; 3L; 4L; 5L; 6L; 6L; 4L]
    let expectedFlows = [4; 4; 2; 0; 4; 4; 0; 6; 4]
    let expectedTotalFlow = 10;
    let maxFlow = new MaxFlow()

    for i=0 to (numArcs-1) do
      let arc = maxFlow.AddArcWithCapacity(tails.[i], heads.[i], capacities.[i])
      if (arc <> i) then
        failwith "Internal error"

    let source = 0;
    let sink = numNodes - 1;
    let solveStatus = maxFlow.Solve(source, sink)

    match solveStatus with
    | x when x = MaxFlow.Status.OPTIMAL ->
        let totalFlow = maxFlow.OptimalFlow();
        totalFlow |> should equal (int64(expectedTotalFlow))

        for i=1 to numArcs do
          (maxFlow.Flow(i-1)) |> should equal (int64(expectedFlows.[i-1]))
    | _ -> ()

