namespace Operations.Research

module Types =
  open System

  type Variable =
      | Boolean of name:string
      | Integer of name:string * lowerBound:int * upperBound:int
      | Real of name:string * lowerBound:float * upperBound:float

  type Operand =
      | Real of float * Variable
      | Integer of int * Variable

  let inline (*) (coeff:obj) (var:Variable) : Operand =
      match coeff with
      | (:? int as i) -> Operand.Integer(i, var)
      | (:? float as f) -> Operand.Real(f, var)

  type Expression = Operand list

  let inline (+) (ops:Expression) (e:Operand) : Expression = List.append ops [e]

  type Constraint = Constraint of Expression * lowerBound:float * upperBound:float

  let inline (<=) (e:Expression) (ub:float)  = Constraint(e, Double.NegativeInfinity, ub)
  let inline (>=) (e:Expression) (lb:float)  = Constraint(e, lb, Double.NegativeInfinity)
  let inline (=) (e:Expression) (v:float)  = Constraint(e, v, v)

  type SolverParams = {
    Name: string
    Variables: Variable list
    Objective: string
    Constraints: Constraint list
  }
