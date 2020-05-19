General resource for using the F# Operations Research library.

## Linear Programming

```fsharp

open System
open Operations.Research.Types
open Operations.Research.Models
open Operations.Research.Solvers.Google.Linear

let x = Variable.Real("x", 0.0, Double.PositiveInfinity)
let y = Variable.Real("y", 0.0, Double.PositiveInfinity)


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
    printfn "%s: %i" (sol.Variables.["x"].Name) (sol.Variables.["x"].Data.toInt)
    printfn "%s: %i" (sol.Variables.["y"].Name) (sol.Variables.["y"].Data.toInt)
| Error e ->
    printfn "%A" e

```

## Constraint Programming

```fsharp
  open System
  open Operations.Research.Types
  open Operations.Research.Models
  open Operations.Research.Solvers.Google.Constraint

  let r = Variable.Integer("rabbits", 0, 100)
  let p = Variable.Integer("pheasants", 0, 100)

  let mdl =
    Model.Default
    |> DecisionVars [r; p]
    |> Constraints [
      r + p === 20
      4*r + 2*p === 56
    ]

  let result = Solve mdl

  match result with
  | Solution sol ->
      printfn "%s: %i" (sol.Variables.["rabbits"].Name) (sol.Variables.["rabbits"].Data.toInt)
      printfn "%s: %i" (sol.Variables.["pheasants"].Name) (sol.Variables.["pheasants"].Data.toInt)
  | Error e ->
    printfn "%A" e
```


## Resources

[Google's OrTools](https://developers.google.com/optimization/)  
[Operations Research (Wikipedia)](https://en.wikipedia.org/wiki/Operations_research)  
[F# Foundation](http://foundation.fsharp.org/)  
[Linear Programming](https://en.wikipedia.org/wiki/Linear_programming)
[Constraint Programming](https://en.wikipedia.org/wiki/Constraint_programming)

