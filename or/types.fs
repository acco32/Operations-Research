namespace Operations.Research

module Types =
  open System


  type Variable1 = {
    Name: string;
    LowerBound: float;
    UpperBound: float;
    Value: float;
  }
  with
    static member Boolean(name) =
      { Name=name; LowerBound=0.0; UpperBound=1.0; Value=0.0 }
    static member Real(name, lowerBound, upperBound) =
      { Name=name; LowerBound=lowerBound; UpperBound=upperBound; Value=0.0 }
    static member Integer(name, lowerBound, upperBound) =
      { Name=name; LowerBound=lowerBound; UpperBound=upperBound; Value=0.0 }


  type Variable =
      | Boolean of name:string * value:bool
      | Integer of name:string * lowerBound:int * upperBound:int * value:int
      | Real of name:string * lowerBound:float * upperBound:float * value:float


  type Operand =
      | Real of float * Variable
      | Integer of int * Variable

  // let inline (*) (coeff:obj) (var:Variable) : Operand =
  //     match coeff with
  //     | (:? int as i) -> Operand.Integer(i, var)
  //     | (:? float as f) -> Operand.Real(f, var)

  // type Expression = Operand list

  // let inline (+) (ops:Expression) (e:Operand) : Expression = List.append ops [e]

  // type Constraint = Constraint of Expression * lowerBound:float * upperBound:float

  // let inline (<==) (e:Expression) (ub:float)  = Constraint(e, Double.NegativeInfinity, ub)
  // let inline (>==) (e:Expression) (lb:float)  = Constraint(e, lb, Double.NegativeInfinity)
  // let inline (==) (e:Expression) (v:float)  = Constraint(e, v, v)


  // type SolverParams = {
  //   Name: string
  //   Variables: Variable list
  //   Objective: string
  //   Constraints: Constraint list
  // }
