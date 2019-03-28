namespace Operations.Research.Solvers


module Google =
  open Operations.Research.Types
  open Operations.Research.Models
  open Google.OrTools.LinearSolver

  let Solve (opts:SolverParams) : SolverResult =
    let solver = new Solver("", Solver.GLOP_LINEAR_PROGRAMMING)

    let convertVar (v:Operations.Research.Types.Variable) : Variable =
      match v with
      | Boolean(b) -> solver.MakeBoolVar(b.Name)
      | Number(n) -> solver.MakeNumVar(n.LowerBound, n.UpperBound, n.Name)

    let convertCons (cnst:Operations.Research.Types.Constraint) =
      match cnst with
      | Operations.Research.Types.Constraint(e, lb, ub) ->
          // e will always be a collection
          let c = solver.MakeConstraint(lb, ub)

          // go through collection and create the coefficients.
          match e with
          | Expression(expr) ->
              expr |> List.iter (fun o ->
                match o with
                | Compound(coeff, var) ->
                    c.SetCoefficient(solver.LookupVariableOrNull(var.Name), coeff)
              )

    let convertObjective (objFcn:Operations.Research.Types.Operand option): Objective =
      let objective = solver.Objective()

      match objFcn with
      | Some(Expression(expr)) ->
          expr |> List.iter (fun o ->
            match o with
            | Compound(coeff, var) ->
                objective.SetCoefficient(solver.LookupVariableOrNull(var.Name), coeff)
            | Value(v) ->
                let var = solver.MakeNumVar(v,v, v.ToString())
                objective.SetCoefficient(var, 1.0)
          )

      objective

    let vars = List.map convertVar opts.Variables
    let cons = List.iter convertCons opts.Constraints
    let objective = convertObjective opts.Objective

    match opts.Goal with
    | Maximize ->
      objective.SetMaximization()
    | Minimize ->
      objective.SetMinimization()
    | _ ->
      failwith "Goal cannot be unset"

    let result = solver.Solve()

    match result with
    | OPTIMAL ->
      let varValues = vars |> List.map (fun (v:Variable) -> Operations.Research.Types.Variable.Set (v.SolutionValue()) (Operations.Research.Types.Variable.Num (v.Name()) (v.Lb()) (v.Ub())) )
      { Variables = varValues ; Objective = solver.Objective().Value(); Optimal = (result.Equals(Solver.OPTIMAL)); Error = None}
    | _ as err ->
      { Variables = List.empty ; Objective = 0.0 ; Optimal = false ; Error = Some(sprintf "Solver returned with error %i" err )}






