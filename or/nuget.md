# F#OR (Operations Research with F#)

Library to solve operations research problems in F#. You can specify your objective and constraints and then use one of the underlying solvers to get an answer. 

## Solver Types

* Linear Programming (Google MIP/LP)
* Constraint Programming (Google CP-SAT)
* Routing (Google Route API)

## Example

```fsharp
open System
open Operations.Research.Types
open Operations.Research.Models
open Operations.Research.Solvers.Google.Linear

let x = Variable.real "x" 0.0 Double.PositiveInfinity |> toExpression
let y = Variable.real "y" 0.0 Double.PositiveInfinity |> toExpression


let mdl =
  Model.Default
  |> DecisionVars [x; y]
  |> Goal Maximize
  |> Objective  (6*x + 2*y + 77)
  |> Constraints [
    3*x + 1*y <== 48
    3*x + 4*y <== 120
    3*x + 1*y >== 36
  ]

let result = SolveWithCustomOptions mdl SolverOptions.Default

match result with
| Solution sol ->
    printfn "Objective: %i" (sol.Objective.toInt)
    printfn "%s: %i" "x" (sol.Variables.["x"])
    printfn "%s: %i" "y" (sol.Variables.["y"])
| Error e ->
    printfn e.Message
```
