namespace Operations.Research.Solvers.Google

open System
open Operations.Research.Types

module Linear =

  // Aliases to disambiguate from name collisions with Google.OrTools.LinearSolver
  type private OrSolver = Google.OrTools.LinearSolver.Solver
  type private OrSolverVar = Google.OrTools.LinearSolver.Variable
  type private OrStatus = Google.OrTools.LinearSolver.Solver.ResultStatus


  module LinearSolverStrategy =
    let GLOP = "GLOP"
    let CLP = "CLP"

  module IntegerSolverStrategy =
    let CBC = "CBC"
    let SCIP = "SCIP"
    let GLPK = "GLPK"


  type SolverOptions =
    { Strategy: string
      TimeLimit: int } // milliseconds; 0 = no limit

    static member Default: SolverOptions =
      { Strategy = LinearSolverStrategy.GLOP
        TimeLimit = 0 }


  let private mapStatus (s: OrStatus) : Status =
    match s with
    | OrStatus.OPTIMAL -> Optimal
    | OrStatus.FEASIBLE -> Feasible
    | OrStatus.INFEASIBLE -> Infeasible
    | OrStatus.UNBOUNDED -> Unbounded
    | _ -> NotSolved

  let private lo (v: float option) : float =
    v |> Option.defaultValue Double.NegativeInfinity

  let private hi (v: float option) : float =
    v |> Option.defaultValue Double.PositiveInfinity

  let private buildVariable (solver: OrSolver) (v: Variable) : OrSolverVar =
    match v.Kind with
    | Boolean -> solver.MakeBoolVar(v.Name)
    | Integer -> solver.MakeIntVar(lo v.Lower, hi v.Upper, v.Name)
    | Real -> solver.MakeNumVar(lo v.Lower, hi v.Upper, v.Name)

  let private requireVar (lookup: Map<string, OrSolverVar>) (name: string) : OrSolverVar =
    match Map.tryFind name lookup with
    | Some sv -> sv
    | None -> failwithf "Solver references unknown variable: %s" name

  let private addConstraint (solver: OrSolver) (lookup: Map<string, OrSolverVar>) (con: Constraint) =
    match con.Kind with
    | NotEqual _ -> failwith "Linear solver does not support NotEqual constraints. Use the constraint solver instead."
    | Range(lower, upper) ->
      // Move the expression's constant into the bounds:
      // (sum c_i x_i) + k in [lower, upper]  <=>  (sum c_i x_i) in [lower - k, upper - k]
      let k = con.Expression.Constant
      let cLo = (lo lower) - k
      let cHi = (hi upper) - k
      let name = con.Name |> Option.defaultValue ""
      let c = solver.MakeConstraint(cLo, cHi, name)

      con.Expression.Coefficients
      |> Map.iter (fun n coeff -> c.SetCoefficient(requireVar lookup n, coeff))

  let private setObjective (solver: OrSolver) (lookup: Map<string, OrSolverVar>) (goal: Goal) (expr: LinearExpression) =
    let obj = solver.Objective()

    expr.Coefficients
    |> Map.iter (fun n coeff -> obj.SetCoefficient(requireVar lookup n, coeff))

    obj.SetOffset(expr.Constant)

    match goal with
    | Maximize -> obj.SetMaximization()
    | Minimize -> obj.SetMinimization()

  let private extractSolution
    (status: Status)
    (varNames: string list)
    (built: Map<string, OrSolverVar>)
    (mdl: Model)
    (solver: OrSolver)
    : Solution =
    match status with
    | Optimal
    | Feasible ->
      let values =
        varNames
        |> List.map (fun n -> n, (Map.find n built).SolutionValue())
        |> Map.ofList

      let objective =
        match mdl.Objective with
        | Some _ -> Some(solver.Objective().Value())
        | None -> None

      { Status = status
        Objective = objective
        Values = values }
    | _ ->
      { Status = status
        Objective = None
        Values = Map.empty }


  let SolveWithCustomOptions (mdl: Model) (opts: SolverOptions) : Solution =
    let solver = OrSolver.CreateSolver(opts.Strategy)

    if isNull solver then
      failwithf "Could not create solver with strategy: %s" opts.Strategy

    if opts.TimeLimit > 0 then
      solver.SetTimeLimit(int64 opts.TimeLimit)

    // Build variables, indexed by name
    let built =
      mdl.Variables
      |> List.map (fun v -> v.Name, buildVariable solver v)
      |> Map.ofList

    // Objective (only when both expression and goal are provided)
    match mdl.Objective, mdl.Goal with
    | Some expr, Some goal -> setObjective solver built goal expr
    | _ -> ()

    // Constraints
    mdl.Constraints |> List.iter (addConstraint solver built)

    // Solve and extract
    let status = mapStatus (solver.Solve())
    let varNames = mdl.Variables |> List.map (fun v -> v.Name)
    extractSolution status varNames built mdl solver

  let Solve (mdl: Model) : Solution =
    SolveWithCustomOptions mdl SolverOptions.Default
