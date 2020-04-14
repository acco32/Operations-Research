namespace Operations.Research.Solvers.Google

module Constraint =
  open System
  open Operations.Research.Types
  open Operations.Research.Models
  open Google.OrTools.Sat


  type SolverOptions = {
    /// Time limit for solver to run. Unit is seconds.
    TimeLimit: int;
  }
  with
  /// Default options to pass to solver
  static member Default =
    { TimeLimit=30 }
  end

  let SolveWithCustomOptions (mdl:Model) (opts:SolverOptions) : SolverResult =

    let model = CpModel()
    let mutable vars = List.Empty

    mdl.Constraints
    |> List.iter (fun (cnsrnt:Operations.Research.Types.Constraint) ->

      let (Constraint(expr, bnds)) = cnsrnt

      let oprndsMap = (expr.Statement) |> List.map ( fun (v:Operand) ->
        match v with
        | Constant(c) ->
            let newVar = model.NewConstant(int64(c.toInt))
            vars <- vars@[newVar]
            int64(1) * newVar
        | Argument(a) ->
            let newVar = model.NewIntVar(int64(a.LowerBound.toInt), int64(a.UpperBound.toInt), a.Name)
            vars <- vars@[newVar]
            int64(1) * newVar
        | CoefficientArgument(c,a) ->
            let newVar = model.NewIntVar(int64(a.LowerBound.toInt), int64(a.UpperBound.toInt), a.Name)
            vars <- vars@[newVar]
            int64(c.toInt) * newVar
      )

      let oprnds = List.reduce (+) oprndsMap

      let cc =
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
        | _, ub, _ ->
            new BoundedLinearExpression(oprnds, int64(ub.toInt), false);

      model.Add(cc) |> ignore
    )

    let solver = CpSolver()
    let result = solver.Solve(model)

    match result with
    | CpSolverStatus.Optimal | CpSolverStatus.Feasible ->

        let varMap =
          vars |> List.map ( fun v ->
              (
                v.Name(),
                Operations.Research.Types.Variable.Number( { Name=(v.Name()); Bounds={Lower=Number.Integer(int(v.Domain.Min())); Upper=Number.Integer(int(v.Domain.Max())); Interval=Interval.Include}; Value=Number.Integer(int(solver.Value(v))) })
              )
          )
          |> Map.ofSeq

        Solution({ Variables = varMap ; Objective = Number.Real(solver.Response.ObjectiveValue); Optimal = false})

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

