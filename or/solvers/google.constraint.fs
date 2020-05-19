namespace Operations.Research.Solvers.Google

module Constraint =
  open System
  open Operations.Research.Types
  open Operations.Research.Models
  open Google.OrTools.Sat


  type SolverOptions = {
    /// Time limit for solver to run. Unit is seconds.
    TimeLimit: int;
    // Number of parallel workers to use. Default is 2.
    SearchWorkers: int;
    // Whether to log search output. Default is FALSE.
    LogSearchProgress: bool;
  }
  with
  /// Default options to pass to solver
  static member Default =
    { TimeLimit=30; SearchWorkers=2; LogSearchProgress=false}
  end

  let SolveWithCustomOptions (mdl:Model) (opts:SolverOptions) : SolverResult =

    let model = CpModel()

    let vars:Map<String, IntVar> =
      mdl.Variables
      |> List.map (fun v -> v.Name, model.NewIntVar(int64(v.LowerBound.toInt), int64(v.UpperBound.toInt), v.Name))
      |> Map.ofList

    mdl.Constraints
    |> List.iter (fun (cnsrnt:Operations.Research.Types.Constraint) ->

      let (Constraint(expr, bnds)) = cnsrnt

      let oprndsMap = (expr.Statement) |> List.map ( fun (v:Operand) ->
        match v with
        | Constant(c) ->
            1L*model.NewConstant(int64(c.toInt))
        | Argument(a) ->
            1L*vars.[a.Name]
        | CoefficientArgument(c,a) ->
            int64(c.toInt) * vars.[a.Name]

      )

      // LHS
      let oprnds = List.reduce (+) oprndsMap

      // LHS & RHS
      let cons =
        match bnds.Lower, bnds.Upper, bnds.Interval with
        | Number.Integer(Int32.MinValue), ub, Include ->
            new BoundedLinearExpression(Int64.MinValue, oprnds, int64(ub.toInt))
        | Number.Real(Double.NegativeInfinity), ub, Include ->
            new BoundedLinearExpression(Int64.MinValue, oprnds, int64(ub.toInt))
        | lb, Number.Real(Double.PositiveInfinity), Include ->
            new BoundedLinearExpression(int64(lb.toInt), oprnds, Int64.MaxValue)
        | lb, Number.Integer(Int32.MaxValue), Include ->
            new BoundedLinearExpression(int64(lb.toInt), oprnds, Int64.MaxValue)
        | _, ub, Exclude ->
            new BoundedLinearExpression(oprnds, int64(ub.toInt), false);
        | _, ub, Include ->
            new BoundedLinearExpression(oprnds, int64(ub.toInt), true);

      model.Add(cons) |> ignore
    )

    let solver = CpSolver()

    solver.StringParameters <-
      List.empty
        |> List.append [(sprintf "max_time_in_seconds:%i" opts.TimeLimit)]
        |> List.append [(sprintf "log_search_progress:%s" (opts.LogSearchProgress.ToString().ToLower()))]
        |> List.append [(sprintf "num_search_workers:%i" opts.SearchWorkers)]
        |> String.concat ","

    let result = solver.Solve(model)

    match result with
    | CpSolverStatus.Optimal | CpSolverStatus.Feasible ->

        let varMap2 =
          vars |> Map.map (fun k v ->
            Operations.Research.Types.Variable.Number( { Name=(v.Name()); Bounds={Lower=Number.Integer(int(v.Domain.Min())); Upper=Number.Integer(int(v.Domain.Max())); Interval=Interval.Include}; Value=Number.Integer(int(solver.Value(v))) })
          )

        Solution({ Variables = varMap2 ; Objective = Number.Real(solver.Response.ObjectiveValue); Optimal = false})

    | CpSolverStatus.Infeasible as err ->
      Error({Code=int(err); Message="Infeasible"})
    | CpSolverStatus.Unknown as err ->
      Error({Code=int(err); Message="Unknown"})
    | CpSolverStatus.ModelInvalid as err ->
      Error({Code=int(err); Message="Model Invalid"})
    | _ as err ->
      Error({Code=int(err); Message="Not Solved"})

  let Solve (mdl:Model) : SolverResult =
    SolveWithCustomOptions mdl SolverOptions.Default

