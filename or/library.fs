namespace Operations.Research

module Models =
  open System
  open Operations.Research.Types

  let inline (<==) (op:Operand) (s:obj) =
    match op, s with
    | Expression(_) as exprs, (:? int as i)  -> Constraint(exprs, {Lower=Number.Integer(0); Upper=Number.Integer(i); Interval=Interval.Include})
    | Expression(_) as exprs, (:? float as f)  -> Constraint(exprs, {Lower=Number.Real(0.); Upper=Number.Real(f); Interval=Interval.Include})
    | _ -> failwith "Cannot use value expression"

  let inline (>==) (op:Operand) (s:obj) =
    match op, s with
    | Expression(_) as exprs, (:? int as i) -> Constraint(exprs, {Lower=Number.Integer(i); Upper=Number.Integer(Int32.MaxValue); Interval=Interval.Include})
    | Expression(_) as exprs, (:? float as f) -> Constraint(exprs, {Lower=Number.Real(f); Upper=Number.Real(Double.PositiveInfinity); Interval=Interval.Include})
    | _ -> failwith "Cannot use value expression"

  let inline (===) (op:Operand) (s:obj) =
    match op, s with
    | Expression(_) as exprs, (:? int as i) -> Constraint(exprs, {Lower=Number.Integer(i); Upper=Number.Integer(i); Interval=Interval.Include})
    | Expression(_) as exprs, (:? float as f) -> Constraint(exprs, {Lower=Number.Real(f); Upper=Number.Real(f); Interval=Interval.Include})
    | _ -> failwith "Cannot use value expression"

  let inline (=/=) (op:Operand) (s:obj) =
    match op, s with
    | Expression(_) as exprs, (:? int as i) -> Constraint(exprs, {Lower=Number.Integer(i); Upper=Number.Integer(i); Interval=Interval.Exclude})
    | _ -> failwith "Can only use integer expressions for this constraint"

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
