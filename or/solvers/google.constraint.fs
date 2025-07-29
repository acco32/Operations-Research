namespace Operations.Research.Solvers.Google

module Constraint =
  open System
  open Google.OrTools.Sat
  open Operations.Research.Types
  open Operations.Research.Models


  type SolverOptions =
    {
      /// Time limit for solver to run. Unit is seconds.
      TimeLimit: int
      // Number of parallel workers to use. Default is 2.
      SearchWorkers: int
      // Whether to log search output. Default is FALSE.
      LogSearchProgress: bool
    }

    /// Default options to pass to solver
    static member Default =
      { TimeLimit = 30
        SearchWorkers = 2
        LogSearchProgress = false }

  let SolveWithCustomOptions (mdl: Model) (opts: SolverOptions) : SolverResult =

    let model = CpModel()

    let vars =
      mdl.Variables
      |> List.map (fun (e: Expression) ->
        let term = e.var ()
        term.Name, model.NewIntVar(int64 (term.Bounds.Lower.toInt), int64 (term.Bounds.Upper.toInt), term.Name))
      |> Map.ofList

    mdl.Constraints
    |> List.iter (fun (cnsrnt: Operations.Research.Types.Constraint) ->

      let (Constraint(expr, bnds)) = cnsrnt

      // LHS
      let oprnds =
        expr.Terms
        |> List.map (fun (trm: Term) ->

          match trm.Bounds.Lower, trm.Bounds.Upper, trm.IsBoolean with
          | _, _, true -> int64 (trm.Coefficient.toInt) * model.NewBoolVar(trm.Name)
          | lb, ub, _ when lb = ub -> 1L * model.NewConstant(int64 (lb.toInt))
          | lb, ub, _ -> int64 (trm.Coefficient.toInt) * vars.[trm.Name])
        |> List.reduce (+)

      // LHS & RHS
      let cons =
        match bnds.Lower, bnds.Upper, bnds.Interval with
        | Number.Integer(Int32.MinValue), ub, Include ->
          new BoundedLinearExpression(Int64.MinValue, oprnds, int64 (ub.toInt))
        | Number.Real(Double.NegativeInfinity), ub, Include ->
          new BoundedLinearExpression(Int64.MinValue, oprnds, int64 (ub.toInt))
        | lb, Number.Real(Double.PositiveInfinity), Include ->
          new BoundedLinearExpression(int64 (lb.toInt), oprnds, Int64.MaxValue)
        | lb, Number.Integer(Int32.MaxValue), Include ->
          new BoundedLinearExpression(int64 (lb.toInt), oprnds, Int64.MaxValue)
        | _, ub, Exclude -> new BoundedLinearExpression(oprnds, int64 (ub.toInt), false)
        | _, ub, Include -> new BoundedLinearExpression(oprnds, int64 (ub.toInt), true)

      model.Add(cons) |> ignore)

    let solver = CpSolver()

    solver.StringParameters <-
      List.empty
      |> List.append [ (sprintf "max_time_in_seconds:%i" opts.TimeLimit) ]
      |> List.append [ (sprintf "log_search_progress:%s" (opts.LogSearchProgress.ToString().ToLower())) ]
      |> List.append [ (sprintf "num_search_workers:%i" opts.SearchWorkers) ]
      |> String.concat ","

    let result = solver.Solve(model)

    match result with
    | CpSolverStatus.Optimal
    | CpSolverStatus.Feasible ->

      let varMap = vars |> Map.map (fun _ v -> float (solver.Value(v)))

      Solution(
        { Variables = varMap
          Objective = Number.Real(solver.Response.ObjectiveValue)
          Optimal = result.Equals(CpSolverStatus.Optimal) }
      )

    | CpSolverStatus.Infeasible as err ->
      Error(
        { Code = int (err)
          Message = "Infeasible" }
      )
    | CpSolverStatus.Unknown as err ->
      Error(
        { Code = int (err)
          Message = "Unknown" }
      )
    | CpSolverStatus.ModelInvalid as err ->
      Error(
        { Code = int (err)
          Message = "Model Invalid" }
      )
    | _ as err ->
      Error(
        { Code = int (err)
          Message = "Not Solved" }
      )

  let Solve (mdl: Model) : SolverResult =
    SolveWithCustomOptions mdl SolverOptions.Default
