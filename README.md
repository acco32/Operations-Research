# Operations Research with F#

Library to solve operations research problems in F\#. You can specify your objective and constraints and then use one of the underlying solvers to get an answer. Currently the only solver used is [Google's OrTools](https://developers.google.com/optimization/).

## Solver Types

- Linear Programming

## Example

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


## Install necessary tools

Run the script to `install-task-runner.sh` to install [tusk](https://github.com/rliebz/tusk). This will install the tool nessary for building the application. Then run the tool to view the task options.

```shell
./tools/tusk
```

## Links

[Google's OrTools](https://developers.google.com/optimization/)
[Operations Research (Wikipedia)](https://en.wikipedia.org/wiki/Operations_research)
[F# Foundation](http://foundation.fsharp.org/)
