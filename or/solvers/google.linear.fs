namespace Operations.Research.Solvers


module Google =
  open Operations.Research.Types
  open Operations.Research.Models
  open Google.OrTools.LinearSolver

  let Solve (opts:SolverParams) : Solver =
    let solver = new Solver("", Solver.CLP_LINEAR_PROGRAMMING)

    let convertVar (v:Operations.Research.Types.Variable) : Variable =
      match v with
      | Boolean(b) -> solver.MakeBoolVar(b.Name)
      | Number(n) -> solver.MakeNumVar(n.LowerBound, n.UpperBound, n.Name)

    let vars = List.map convertVar opts.Variables

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
                | Compound(coeff,var) ->
                    c.SetCoefficient(solver.LookupVariableOrNull(var.Name), coeff)
                // | Value(val) ->
              )

    List.iter convertCons opts.Constraints

    solver





