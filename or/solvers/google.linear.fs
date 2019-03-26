namespace Operations.Research.Solvers


module Google =
  open Operations.Research.Types
  open Operations.Research.Models
  open Google.OrTools.LinearSolver

  let Solve (opts:SolverParams) : SolverResult =
    let solver = new Solver("", Solver.CLP_LINEAR_PROGRAMMING)

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
                // | Value(v) ->
                //     c.SetCoefficient(solver.LookupVariableOrNull(""), v)
              )

    let convertObjective (objFcn:Operations.Research.Types.Operand option): Objective =
      let objective = solver.Objective()

      match objFcn with
      | Some(Expression(expr)) ->
          expr |> List.iter (fun o ->
            match o with
            | Compound(coeff, var) ->
                objective.SetCoefficient(solver.LookupVariableOrNull(var.Name), coeff)
            // | Value(v) ->
            //     objective.SetCoefficient(solver.LookupVariableOrNull(""), v)
          )

      objective

    let vars = List.map convertVar opts.Variables
    let cons = List.iter convertCons opts.Constraints
    let objective = convertObjective opts.Objective

    let result = solver.Solve()

    { Variables = List.empty; Objective = solver.Objective().Value(); Optimal = (result == Solver.OPTIMAL) }





