namespace Operations.Research

module Models =
  open System
  open Operations.Research.Types

  let inline (<==) (exp:Expression) (s:obj) =
    match s with
    | (:? int as i)  -> Constraint(exp, {Lower=Number.Integer(Int32.MinValue); Upper=Number.Integer(i); Interval=Interval.Include})
    | (:? float as f)  -> Constraint(exp, {Lower=Number.Real(Double.NegativeInfinity); Upper=Number.Real(f); Interval=Interval.Include})
    | _ -> failwith "Cannot use value expression"


  let inline (>==) (exp:Expression) (s:obj) =
    match s with
    | (:? int as i) -> Constraint(exp, {Lower=Number.Integer(i); Upper=Number.Integer(Int32.MaxValue); Interval=Interval.Include})
    | (:? float as f) -> Constraint(exp, {Lower=Number.Real(f); Upper=Number.Real(Double.PositiveInfinity); Interval=Interval.Include})
    | _ -> failwith "Cannot use value expression"


  let inline (===) (exp:Expression) (s:obj) =
    match s with
    | (:? int as i) -> Constraint(exp, {Lower=Number.Integer(i); Upper=Number.Integer(i); Interval=Interval.Include})
    | (:? float as f) -> Constraint(exp, {Lower=Number.Real(f); Upper=Number.Real(f); Interval=Interval.Include})
    | _ -> failwith "Cannot use value expression"


  let inline (=/=) (exp:Expression) (s:obj) =
    match s with
    | (:? int as i) -> Constraint(exp, {Lower=Number.Integer(i); Upper=Number.Integer(i); Interval=Interval.Exclude})
    | _ -> failwith "Can only use integer expressions for this constraint"


  let DecisionVars (vars:Expression list) (mdl:Model) =
    {mdl with Variables=vars}


  let Constraint (con:Constraint) (mdl:Model) =
    match mdl.Constraints with
    | [] -> {mdl with Constraints=[con]}
    | cn -> {mdl with Constraints=cn |> List.append [con] }


  let Constraints (con:Constraint list) (mdl:Model) =
    {mdl with Constraints=con}


  let Objective (exp:Expression) (mdl:Model) =
    {mdl with Objective=Some(exp)}


  let Goal (goal:Goal) (mdl:Model) =
    match goal with
    | Unset -> failwith "Goal must be set"
    | _ -> {mdl with Goal=goal}


  let Matrix (m:float list list) (lb:float list) (ub:float list) (mdl:Model) =

    let vars = List.map (fun (v:Expression) -> v.var()) (mdl.Variables)

    let createConstraintFromRow (row:float list) (lb:float) (ub:float) =
      let operands = List.map2 (fun (coeff:float) (v:Term) -> {v with Coefficient=Number.Real(coeff)} |> toExpression ) row vars
      let exp = List.reduce (+) operands
      let bnds = {Lower=Number.Real(lb); Upper=Number.Real(ub); Interval=Interval.Include}
      Types.Constraint(exp, bnds)

    let cons = List.map3 (fun row lowerBound upperBound -> createConstraintFromRow row lowerBound upperBound) m lb ub
    {mdl with Constraints = cons}


  let MatrixEq (m:float list list) (vec:float list) (mdl:Model) =

    let vars = List.map (fun (v:Expression) -> v.var()) (mdl.Variables)

    let createConstraintFromRow (row:float list) (vecEq:float) =
      let operands = List.map2 (fun (coeff:float) (v:Term) -> {v with Coefficient=Number.Real(coeff)} |> toExpression ) row vars
      List.reduce (+) operands === vecEq

    let cons = List.map2 (fun row vector -> createConstraintFromRow row vector) m vec
    {mdl with Constraints = cons}
