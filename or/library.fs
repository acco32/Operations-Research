namespace Operations.Research


module LinearProgramming =
  open System
  open Operations.Research.Types

  // Create a new Boolean Variable. Initial value is false.
  let BooleanVariable name = Variable.Boolean(name, false)

  // Create a new Integer Variable. Initial value is lower bound.
  let IntegerVariable name lowerBound upperBound = Variable.Integer(name, lowerBound, upperBound, lowerBound)

  // Create a new Real Valued Variable. Initial value is lower bound.
  let RealVariable name lowerBound upperBound = Variable.Real(name, lowerBound, upperBound, lowerBound)

  let inline (*) (coeff:obj) (var:Variable) : Operand =
      match coeff with
      | (:? int as i) -> Operand.Integer(i, var)
      | (:? float as f) -> Operand.Real(f, var)

  type Expression = Operand list

  let inline (+) (ops:Expression) (e:Operand) : Expression = List.append ops [e]

  type Constraint = Constraint of Expression * lowerBound:float * upperBound:float

  let inline (<==) (e:Expression) (ub:float)  = Constraint(e, Double.NegativeInfinity, ub)
  let inline (>==) (e:Expression) (lb:float)  = Constraint(e, lb, Double.NegativeInfinity)
  let inline (==) (e:Expression) (v:float)  = Constraint(e, v, v)

