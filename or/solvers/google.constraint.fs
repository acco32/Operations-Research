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

    mdl.Constraints
    |> List.iter (fun (cnsrnt:Operations.Research.Types.Constraint) ->

      let (Constraint(expr, bnds)) = cnsrnt

      // map operands to ORTOOLS operands
      let oprnds = (expr.Statement) |> List.map ( fun (v:Operand) ->
        match v with
        | Constant(c) ->
            int64(1) * model.NewConstant(int64(c.toInt))
        | Argument(a) ->
            int64(1) * model.NewIntVar(int64(a.LowerBound.toInt), int64(a.UpperBound.toInt), a.Name)
        | CoefficientArgument(c,a) ->
            int64(c.toInt) * model.NewIntVar(int64(a.LowerBound.toInt), int64(a.UpperBound.toInt), a.Name)
      )

      // map the operand then add the constraint to the model
      model.AddLinearConstraint((List.reduce (+) oprnds), int64(bnds.Lower.toInt), int64(bnds.Upper.toInt))

    )

    let solver = CpSolver()

    Solution({ Variables = Map.empty ; Objective = Number.Integer(0); Optimal = true})

  // let Solve (mdl:Model) : SolverResult =
  //   SolveWithCustomOptions mdl SolverOptions.Default

