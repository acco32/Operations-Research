namespace Operations.Research

module Models =

  open Operations.Research.Types

  let inline (<==) (e: LinearExpression) (v: ^a) : Constraint =
    let value: float = float v

    { Name = None
      Expression = e
      Kind = Range(None, Some value) }

  let inline (>==) (e: LinearExpression) (v: ^a) : Constraint =
    let value: float = float v

    { Name = None
      Expression = e
      Kind = Range(Some value, None) }

  let inline (===) (e: LinearExpression) (v: ^a) : Constraint =
    let value: float = float v

    { Name = None
      Expression = e
      Kind = Range(Some value, Some value) }

  let inline (=/=) (e: LinearExpression) (v: ^a) : Constraint =
    let value: float = float v

    { Name = None
      Expression = e
      Kind = NotEqual value }

  let inline (<->) (e: LinearExpression) (bounds: ^a * ^b) : Constraint =
    let lower: float = float (fst bounds)
    let upper: float = float (snd bounds)

    if lower > upper then
      failwith "Lower bound must be less than or equal to upper bound"

    { Name = None
      Expression = e
      Kind = Range(Some lower, Some upper) }


  let DecisionVars (vars: Variable list) (mdl: Model) : Model = { mdl with Variables = vars }

  let Goal (goal: Goal) (mdl: Model) : Model = { mdl with Goal = Some goal }

  let Objective (exp: LinearExpression) (mdl: Model) : Model = { mdl with Objective = Some exp }

  let Constraint (con: Constraint) (mdl: Model) : Model =
    { mdl with
        Constraints = mdl.Constraints @ [ con ] }

  let Constraints (cons: Constraint list) (mdl: Model) : Model = { mdl with Constraints = cons }


  let private rowToConstraint
    (vars: Variable list)
    (row: float list)
    (lower: float option)
    (upper: float option)
    : Constraint =
    let expr =
      List.zip row vars
      |> List.fold (fun acc (coeff, v) -> acc + coeff * v) LinearExpression.zero

    { Name = None
      Expression = expr
      Kind = Range(lower, upper) }

  let Matrix (m: float list list) (lb: float list) (ub: float list) (mdl: Model) : Model =
    let cons =
      List.map3 (fun row l u -> rowToConstraint mdl.Variables row (Some l) (Some u)) m lb ub

    { mdl with Constraints = cons }

  let MatrixEq (m: float list list) (vec: float list) (mdl: Model) : Model =
    let cons =
      List.map2 (fun row v -> rowToConstraint mdl.Variables row (Some v) (Some v)) m vec

    { mdl with Constraints = cons }
