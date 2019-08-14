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

  let DecisionVars (vars:Variable list) (mdl:Model) =
    {mdl with Variables=vars}

  let Constraint (con:Constraint) (mdl:Model) =
    match mdl.Constraints with
    | [] -> {mdl with Constraints=[con]}
    | cn -> {mdl with Constraints=cn |> List.append [con] }

  let Constraints (con:Constraint list) (mdl:Model) =
    {mdl with Constraints=con }

  let Objective (obj:Operand) (mdl:Model) =
    match obj with
    | Expression(_) as o -> {mdl with Objective=Some(o)}
    | _ -> failwith "Expression is the only acceptable Objective Function parameter"

  let Goal (goal:Goal) (mdl:Model) =
    match goal with
    | Unset -> failwith "Goal must be set"
    | _ -> {mdl with Goal=goal}

  let Matrix (m:float list list) (lb:float list) (ub:float list) (mdl:Model) =

    let createConstraintFromRow (row:float list) (ub:float) =
      let operands = List.map2 (fun (coeff:float) (v:Variable) -> coeff * v) row (mdl.Variables)
      List.reduce (+) operands <== ub

    let cons = List.map2 (fun row upperBound -> createConstraintFromRow row upperBound) m ub
    {mdl with Constraints = cons}

  let MatrixEq (m:float list list) (vec:float list) (mdl:Model) =

    let createConstraintFromRow (row:float list) (vecEq:float) =
      let operands = List.map2 (fun (coeff:float) (v:Variable) -> coeff * v) row (mdl.Variables)
      List.reduce (+) operands === vecEq

    let cons = List.map2 (fun row vector -> createConstraintFromRow row vector) m vec
    {mdl with Constraints = cons}

  /// Special representation of zero as a Variable
  let Zero() = Variable.Num (sprintf "Zero-%s" (Guid.NewGuid().ToString("N").Substring(0,8))) 0.0 0.0
