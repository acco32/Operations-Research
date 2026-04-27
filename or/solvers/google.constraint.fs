namespace Operations.Research.Solvers.Google

open System
open Operations.Research.Types

module Constraint =

  type private CpModel = Google.OrTools.Sat.CpModel
  type private CpSolver = Google.OrTools.Sat.CpSolver
  type private CpSolverStatus = Google.OrTools.Sat.CpSolverStatus
  type private IntVar = Google.OrTools.Sat.IntVar
  type private LinearExpr = Google.OrTools.Sat.LinearExpr

  type SolverOptions =
    { TimeLimit: int // seconds; 0 = no limit
      SearchWorkers: int // 0 = solver default
      LogSearchProgress: bool }

    static member Default: SolverOptions =
      { TimeLimit = 0
        SearchWorkers = 0
        LogSearchProgress = false }

  let private mapStatus (s: CpSolverStatus) : Status =
    match s with
    | CpSolverStatus.Optimal -> Optimal
    | CpSolverStatus.Feasible -> Feasible
    | CpSolverStatus.Infeasible -> Infeasible
    | _ -> NotSolved

  let private toLong (x: float) : int64 =
    if Double.IsInfinity x || Double.IsNaN x then
      failwithf "CP solver requires finite values, got: %f" x

    if x <> floor x then
      failwithf "CP solver requires integer values, got: %f" x

    int64 x

  let private boundLow (v: float option) : int64 =
    v |> Option.map toLong |> Option.defaultValue Int64.MinValue

  let private boundHigh (v: float option) : int64 =
    v |> Option.map toLong |> Option.defaultValue Int64.MaxValue

  let private buildVariable (model: CpModel) (v: Variable) : IntVar =
    match v.Kind with
    | Boolean -> model.NewBoolVar(v.Name)
    | Integer -> model.NewIntVar(boundLow v.Lower, boundHigh v.Upper, v.Name)
    | Real -> failwithf "CP solver does not support Real variables: %s" v.Name

  let private requireVar (lookup: Map<string, IntVar>) (name: string) : IntVar =
    match Map.tryFind name lookup with
    | Some sv -> sv
    | None -> failwithf "Solver references unknown variable: %s" name

  /// Build a LinearExpr from our LinearExpression (constant included).
  let private buildExpr (lookup: Map<string, IntVar>) (e: LinearExpression) : LinearExpr =
    let b = LinearExpr.NewBuilder()

    e.Coefficients
    |> Map.iter (fun n c -> b.AddTerm(requireVar lookup n, toLong c) |> ignore)

    if e.Constant <> 0.0 then
      b.Add(toLong e.Constant) |> ignore

    b :> LinearExpr

  let private addConstraint (model: CpModel) (lookup: Map<string, IntVar>) (con: Constraint) =
    match con.Kind with
    | NotEqual v ->
      // F# can't use C#'s != operator directly; call op_Inequality explicitly.
      let expr = buildExpr lookup con.Expression
      let bound = LinearExpr.op_Inequality (expr, toLong v)
      model.Add(bound) |> ignore
    | Range(lo, hi) ->
      let expr = buildExpr lookup con.Expression
      model.AddLinearConstraint(expr, boundLow lo, boundHigh hi) |> ignore

  let private setObjective (model: CpModel) (lookup: Map<string, IntVar>) (goal: Goal) (expr: LinearExpression) =
    let e = buildExpr lookup expr

    match goal with
    | Maximize -> model.Maximize(e)
    | Minimize -> model.Minimize(e)

  let private setOptions (solver: CpSolver) (opts: SolverOptions) =
    let parts = ResizeArray<string>()

    if opts.TimeLimit > 0 then
      parts.Add(sprintf "max_time_in_seconds:%d.0" opts.TimeLimit)

    if opts.SearchWorkers > 0 then
      parts.Add(sprintf "num_search_workers:%d" opts.SearchWorkers)

    if opts.LogSearchProgress then
      parts.Add("log_search_progress:true")

    if parts.Count > 0 then
      solver.StringParameters <- String.concat " " parts

  let private extractSolution
    (status: Status)
    (varNames: string list)
    (built: Map<string, IntVar>)
    (mdl: Model)
    (solver: CpSolver)
    : Solution =
    match status with
    | Optimal
    | Feasible ->
      let values =
        varNames
        |> List.map (fun n -> n, float (solver.Value(Map.find n built)))
        |> Map.ofList

      let objective =
        match mdl.Objective with
        | Some _ -> Some solver.ObjectiveValue
        | None -> None

      { Status = status
        Objective = objective
        Values = values }
    | _ ->
      { Status = status
        Objective = None
        Values = Map.empty }


  let SolveWithCustomOptions (mdl: Model) (opts: SolverOptions) : Solution =
    let model = CpModel()

    let built =
      mdl.Variables |> List.map (fun v -> v.Name, buildVariable model v) |> Map.ofList

    match mdl.Objective, mdl.Goal with
    | Some expr, Some goal -> setObjective model built goal expr
    | _ -> ()

    mdl.Constraints |> List.iter (addConstraint model built)

    let solver = new CpSolver()
    setOptions solver opts

    let status = mapStatus (solver.Solve(model))
    let varNames = mdl.Variables |> List.map (fun v -> v.Name)
    extractSolution status varNames built mdl solver

  let Solve (mdl: Model) : Solution =
    SolveWithCustomOptions mdl SolverOptions.Default
