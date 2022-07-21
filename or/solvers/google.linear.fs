namespace Operations.Research.Solvers.Google

module Linear =
  open System
  open Operations.Research.Types
  open Operations.Research.Models
  open Google.OrTools.LinearSolver


  type SolverStrategy = {
    Name:string;
    Id: int
  }

  module LinearSolverStrategy =
    /// Coin-Or (Recommended default)
    let CLP = {Name="CLP_LINEAR_PROGRAMMING"; Id=0}

    /// GNU Linear Programming Kit
    let GLPK = {Name="GLPK_LINEAR_PROGRAMMING"; Id=1}

    /// Google Linear Optimization
    let GLOP = {Name="GLOP_LINEAR_PROGRAMMING"; Id=2}

    /// Gurobi Optimizer
    let GUROBI = {Name="GUROBI_LINEAR_PROGRAMMING"; Id=6}

    /// IBM CPLEX
    let CPLEX = {Name="CPLEX_LINEAR_PROGRAMMING"; Id=10}

  module IntegerSolverStrategy =
    /// Solving Constraint Integer Programs (Recommended default)
    let SCIP = {Name="SCIP_MIXED_INTEGER_PROGRAMMING"; Id=3}

    /// GNU Linear Programming Kit
    let GLPK = {Name="GLPK_MIXED_INTEGER_PROGRAMMING"; Id=4}

    /// Coin-Or Branch and Cut
    let CBC = {Name="CBC_MIXED_INTEGER_PROGRAMMING"; Id=5}

    /// Gurobi Optimizer
    let GUROBI = {Name="GUROBI_MIXED_INTEGER_PROGRAMMING"; Id=7}

    /// IBM CPLEX
    let CPLEX = {Name="CPLEX_MIXED_INTEGER_PROGRAMMING"; Id=11}

    /// Binary Optimizer
    let BOP = {Name="BOP_INTEGER_PROGRAMMING"; Id=12}

  type SolverOptions = {
    /// Time limit for solver to run. Unit is seconds.
    TimeLimit: int;
    /// Solver strategy to pass to solver. Can be of type LinearSolverStrategy or IntegerSolverStrategy
    Strategy: SolverStrategy;
  }
  with
  /// Default options to pass to solver
  static member Default =
    { TimeLimit=30; Strategy=LinearSolverStrategy.GLOP }
  end

  let SolveWithCustomOptions (mdl:Model) (opts:SolverOptions) : SolverResult =

    let solver = Solver.CreateSolver(opts.Strategy.Name)

    let (|IntegerStrategy|LinearStrategy|) strtgy =
      if strtgy = IntegerSolverStrategy.BOP then IntegerStrategy
      elif strtgy = IntegerSolverStrategy.SCIP then IntegerStrategy
      elif strtgy = IntegerSolverStrategy.GLPK then IntegerStrategy
      elif strtgy = IntegerSolverStrategy.GUROBI then IntegerStrategy
      elif strtgy = IntegerSolverStrategy.CBC then IntegerStrategy
      elif strtgy = IntegerSolverStrategy.CPLEX then IntegerStrategy
      elif strtgy = LinearSolverStrategy.CPLEX then LinearStrategy
      elif strtgy = LinearSolverStrategy.GUROBI then LinearStrategy
      elif strtgy = LinearSolverStrategy.GLOP then LinearStrategy
      elif strtgy = LinearSolverStrategy.GLPK then LinearStrategy
      elif strtgy = LinearSolverStrategy.CLP then LinearStrategy
      else failwith "Unknown strategy in solver parameters"

    let mutable vars = List.Empty

    let vars = mdl.Variables |> List.map ( fun (e:Expression) ->
        let term = e.var()
        match term.IsBoolean with
        | true -> solver.MakeBoolVar(term.Name)
        | false -> solver.MakeNumVar(term.Bounds.Lower.toFloat, term.Bounds.Upper.toFloat,term.Name)
    )


    mdl.Constraints
    |> List.iter (fun (cnsrnt:Operations.Research.Types.Constraint) ->

      let (Operations.Research.Types.Constraint(expr, bnds)) = cnsrnt

      match bnds.Interval with
      | Include ->
          let con = solver.MakeConstraint(bnds.Lower.toFloat, bnds.Upper.toFloat)

          expr.Terms |> List.iter (fun trm ->

              match isNull(solver.LookupVariableOrNull(trm.Name)) with
              | true ->
                  con.SetCoefficient(solver.MakeNumVar(trm.Bounds.Lower.toFloat, trm.Bounds.Upper.toFloat, trm.Name), 1.0)
              | false  ->
                  con.SetCoefficient(solver.LookupVariableOrNull(trm.Name), trm.Coefficient.toFloat)
          )

      | Exclude ->
          failwithf "Constraint not valid for this type of strategy: %s" opts.Strategy.Name

    )


    let objective = solver.Objective()

    match mdl.Objective with
    | Some(expr) ->
        expr.Terms
        |> List.iter (fun trm ->

              match isNull(solver.LookupVariableOrNull(trm.Name)) with
              | true ->
                  objective.SetCoefficient(solver.MakeNumVar(trm.Bounds.Lower.toFloat, trm.Bounds.Upper.toFloat, trm.Name), 1.0)
              | false  ->
                  objective.SetCoefficient(solver.LookupVariableOrNull(trm.Name), trm.Coefficient.toFloat)

        )
    | None -> failwith "Objective Function cannot be empty for Linear Solver"


    match mdl.Goal with
    | Maximize ->
        objective.SetMaximization()
    | Minimize ->
        objective.SetMinimization()
    | _ ->
        failwith "Goal cannot be unset"

    let result = solver.Solve()

    match result with
    | Solver.ResultStatus.OPTIMAL | Solver.ResultStatus.FEASIBLE ->

      let varMap = List.fold (fun (m:Map<string,float>) (v:Variable) ->  m.Add( v.Name(), v.SolutionValue() ) ) Map.empty vars

      match opts.Strategy with
      | LinearStrategy -> Solution({ Variables = varMap ; Objective = Number.Real(solver.Objective().Value()); Optimal = (result.Equals(Solver.ResultStatus.OPTIMAL))})
      | IntegerStrategy -> Solution({ Variables = varMap ; Objective = Number.Integer(int(solver.Objective().Value())); Optimal = (result.Equals(Solver.ResultStatus.OPTIMAL))})

    | Solver.ResultStatus.INFEASIBLE as err ->
      Error({Code=int(err); Message="Infeasible"})
    | Solver.ResultStatus.UNBOUNDED as err ->
      Error({Code=int(err); Message="Unbounded"})
    | Solver.ResultStatus.ABNORMAL as err ->
      Error({Code=int(err); Message="Abnormal"})
    | _ as err ->
      Error({Code=int(err); Message="Not Solved"})

  let Solve (mdl:Model) : SolverResult =
    SolveWithCustomOptions mdl SolverOptions.Default

