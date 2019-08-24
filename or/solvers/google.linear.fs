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

    let solver = Solver.CreateSolver("", opts.Strategy.Name)

    let mutable vars = List.Empty

    let convertVar (v:Operations.Research.Types.Variable) : Variable =
      match v with
      | Boolean(b) -> solver.MakeBoolVar(b.Name)
      | Number(n) ->
          match n.Value.Number with
          | Number.Real(r) ->
              solver.MakeNumVar(n.Bounds.Lower.toFloat, n.Bounds.Upper.toFloat, n.Name)
          | Number.Integer(i) ->
              solver.MakeIntVar(n.Bounds.Lower.toFloat, n.Bounds.Upper.toFloat, n.Name)

    let convertCons (cnst:Operations.Research.Types.Constraint) =
      match cnst with
      | Operations.Research.Types.Constraint(e, b) ->
          // e will always be a collection
          let c = solver.MakeConstraint(b.Lower.toFloat, b.Upper.toFloat)

          // go through collection and create the coefficients.
          match e with
          | Expression(expr) ->
              expr |> List.iter (fun o ->
                match o with
                | Compound(coeff, var) ->
                    c.SetCoefficient(solver.LookupVariableOrNull(var.Name), coeff.toFloat)
                | Value(v) ->
                    let newVar = solver.MakeNumVar(v.toFloat, v.toFloat, v.ToString())
                    vars <- vars@[newVar]
                    c.SetCoefficient(newVar, 1.0)
              )
          | _ -> ()

    let convertObjective (objFcn:Operations.Research.Types.Operand option): Objective =
      let objective = solver.Objective()

      match objFcn with
      | Some(Expression(expr)) ->
          expr |> List.iter (fun o ->
            match o with
            | Compound(coeff, var) ->
                objective.SetCoefficient(solver.LookupVariableOrNull(var.Name), coeff.toFloat)
            | Value(v) ->
                let newVar = solver.MakeNumVar(v.toFloat, v.toFloat, v.ToString())
                vars <- vars @ [newVar]
                objective.SetCoefficient(newVar, 1.0)
          )

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

      let createVarMap (m:Map<string,Operations.Research.Types.Variable>) (v:Variable) =
        let result = v.SolutionValue()
        let resultVar = Operations.Research.Types.Variable.Number( { Name=(v.Name()); Bounds={Lower=Number.Real(v.Lb()); Upper=Number.Real(v.Ub())}; Value=VariableDataValue.Real(Number.Real(0.0)) })
        m.Add( v.Name(), (Operations.Research.Types.Variable.Set result  resultVar))

      let varMap = List.fold createVarMap Map.empty vars

      Solution({ Variables = varMap ; Objective = solver.Objective().Value(); Optimal = (result.Equals(Solver.ResultStatus.OPTIMAL))})
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




