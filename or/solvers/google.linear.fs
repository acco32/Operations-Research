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

    let convertVar (v:Operations.Research.Types.Variable) : Variable =
      match v with
      | Boolean(b) -> solver.MakeBoolVar(b.Name)
      | Number(n) ->
          match n.Value with
          | Number.Real(r) ->
              solver.MakeNumVar(n.Bounds.Lower.toFloat, n.Bounds.Upper.toFloat, n.Name)
          | Number.Integer(i) ->
              solver.MakeIntVar(n.Bounds.Lower.toFloat, n.Bounds.Upper.toFloat, n.Name)

    let convertCons (cnst:Operations.Research.Types.Constraint) =
      match cnst with
      | Operations.Research.Types.Constraint(e, b) ->
          match b.Interval with
          | Include ->
              let con = solver.MakeConstraint(b.Lower.toFloat, b.Upper.toFloat)

              e.Statement |> List.iter (fun o ->
                match o with
                | Constant(c) ->
                    let newVar = solver.MakeNumVar(c.toFloat, c.toFloat, c.ToString())
                    vars <- vars@[newVar]
                    con.SetCoefficient(newVar, 1.0)
                | Argument(a) ->
                    con.SetCoefficient(solver.LookupVariableOrNull(a.Name), 1.0)
                | CoefficientArgument(c,a) ->
                    con.SetCoefficient(solver.LookupVariableOrNull(a.Name), c.toFloat)
              )

          | Exclude ->
              failwithf "Constraint not valid for this type of strategy: %s" opts.Strategy.Name

    let convertObjective (objFcn:Operations.Research.Types.Expression option): Objective =
      let objective = solver.Objective()

      match objFcn with
      | Some(expr) ->
          expr.Statement |> List.iter (fun o ->
            match o with
            | Constant(c) ->
                let newVar = solver.MakeNumVar(c.toFloat, c.toFloat, c.ToString())
                vars <- vars @ [newVar]
                objective.SetCoefficient(newVar, 1.0)
            | Argument(a) ->
                objective.SetCoefficient(solver.LookupVariableOrNull(a.Name), 1.0)
            | CoefficientArgument(c,a) ->
                objective.SetCoefficient(solver.LookupVariableOrNull(a.Name), c.toFloat)
          )
        | None -> failwith "Objective Function cannot be empty for Linear Solver"

      objective


    vars <- List.map convertVar mdl.Variables
    let cons = List.iter convertCons mdl.Constraints
    let objective = convertObjective mdl.Objective

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

      match opts.Strategy with
      | LinearStrategy ->

          let createVarMap (m:Map<string,Operations.Research.Types.Variable>) (v:Variable) =
            let result = v.SolutionValue()
            let resultVar = Operations.Research.Types.Variable.Number( { Name=(v.Name()); Bounds={Lower=Number.Real(v.Lb()); Upper=Number.Real(v.Ub()); Interval=Interval.Include}; Value=Number.Real(0.0) })
            m.Add( v.Name(), resultVar.Set(result) )

          let varMap = List.fold createVarMap Map.empty vars

          Solution({ Variables = varMap ; Objective = Number.Real(solver.Objective().Value()); Optimal = (result.Equals(Solver.ResultStatus.OPTIMAL))})

      | IntegerStrategy ->

          let createVarMap (m:Map<string,Operations.Research.Types.Variable>) (v:Variable) =
            let result = int(v.SolutionValue())
            let resultVar = Operations.Research.Types.Variable.Number( { Name=(v.Name()); Bounds={Lower=Number.Integer(int(v.Lb())); Upper=Number.Integer(int(v.Ub())); Interval=Interval.Include}; Value=Number.Integer(0) })
            m.Add( v.Name(), resultVar.Set(result) )

          let varMap = List.fold createVarMap Map.empty vars

          Solution({ Variables = varMap ; Objective = Number.Integer(int(solver.Objective().Value())); Optimal = (result.Equals(Solver.ResultStatus.OPTIMAL))})

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




