namespace Operations.Research

module Models =
  open System
  open Operations.Research.Types

  let inline (<==) (op:Operand) (f:float) =
    match op with
    | Compound(_,_) as con -> Constraint(Expression[con], Double.NegativeInfinity, f)
    | Expression(_) as exprs -> Constraint(exprs, Double.NegativeInfinity, f)
    | _ -> failwith "Cannot use value expression"

  let inline (>==) (op:Operand) (f:float) =
    match op with
    | Compound(_,_) as con -> Constraint(Expression[con], f, Double.PositiveInfinity)
    | Expression(_) as exprs -> Constraint(exprs, f, Double.PositiveInfinity)
    | _ -> failwith "Cannot use value expression"

  let inline (===) (op:Operand) (f:float) =
    match op with
    | Compound(_,_) as con -> Constraint(Expression[con], f, f)
    | Expression(_) as exprs -> Constraint(exprs, f, f)
    | _ -> failwith "Cannot use value expression"

  let DecisionVars (vars:Variable list) (opts:SolverParams) =
    {opts with Variables=vars}

  let Constraint (con:Constraint) (opts:SolverParams) =
    match opts.Constraints with
    | [] -> {opts with Constraints=[con]}
    | cn -> {opts with Constraints=cn |> List.append [con] }

  let Objective (obj:Operand) (opts:SolverParams) =
    match obj with
    | Expression(_) as o -> {opts with Objective=Some(o)}
    | _ -> failwith "Expression is the only acceptable Objective Function parameter"

  let Goal (goal:Goal) (opts:SolverParams) =
    match goal with
    | Unset -> failwith "Goal must be set"
    | _ -> {opts with Goal=goal}

  let Matrix (m:float list list) (lb:float list) (ub:float list) (opts:SolverParams) =

    let createConstraintFromRow (row:float list) (ub:float) =
      let operands = List.map2 (fun (coeff:float) (v:Variable) -> coeff * v) row (opts.Variables)
      List.reduce (+) operands <== ub

    let cons = List.map2 (fun row upperBound -> createConstraintFromRow row upperBound) m ub
    {opts with Constraints = cons}

  let MatrixEq (m:float list list) (vec:float list) (opts:SolverParams) =

    let createConstraintFromRow (row:float list) (vecEq:float) =
      let operands = List.map2 (fun (coeff:float) (v:Variable) -> coeff * v) row (opts.Variables)
      List.reduce (+) operands === vecEq

    let cons = List.map2 (fun row vector -> createConstraintFromRow row vector) m vec
    {opts with Constraints = cons}


