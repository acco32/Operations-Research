namespace Operations.Research


module LinearProgramming =
  open System
  open Operations.Research.Types

  let inline (<==) (op:Operand) (f:float) =
    match op with
    | Compound(c,v) as con -> Constraint(Expression[con], Double.NegativeInfinity, f)
    | Expression(l) as exprs -> Constraint(exprs, Double.NegativeInfinity, f)
    | _ -> failwith "Cannot use value expression"

  let inline (>==) (op:Operand) (f:float) =
    match op with
    | Compound(c,v) as con -> Constraint(Expression[con], f, Double.PositiveInfinity)
    | Expression(l) as exprs -> Constraint(exprs, f, Double.PositiveInfinity)
    | _ -> failwith "Cannot use value expression"

  let inline (==) (op:Operand) (f:float) =
    match op with
    | Compound(c,v) as con -> Constraint(Expression[con], f, f)
    | Expression(l) as exprs -> Constraint(exprs, f, f)
    | _ -> failwith "Cannot use value expression"

  let DecisionVars (vars:Variable list) (opts:SolverParams) =
    {opts with Variables=vars}

  let Constraint (con:Constraint) (opts:SolverParams) =
    match opts.Constraints with
    | [] -> {opts with Constraints=[con]}
    | cn -> {opts with Constraints=(cn |> List.append [con] )}



// how to hide an empire: a history of a greater United States