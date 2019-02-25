namespace Operations.Research

module Types =
  open System

  type Variable =
    | Boolean of name:string * value:bool
    | Number of name:string * lowerBound:float * upperBound:float * value:float
    static member Bool (name:string) =
      Boolean(name, false)
    static member Num (name:string) (lowerBound:float) (upperBound:float) =
      Number(name, lowerBound, upperBound, 0.0)
    static member (*) (c:float, v:Variable) = Compound(c, v)
    member this.Set (value) =
      Boolean("", false)
      // pattern match on value's type then ensure it is not out of bounds then set
  and Operand =
    | Compound of float * Variable
    | Value of float
    | Expression of Operand list
    static member (+) (o1:Operand, o2:Operand) =
      match o1,o1 with
      | Expression(a), Expression(b) -> Expression( a @ b )
      | Expression(l), Value(v) -> Expression( l @ [Value(v)]  )
      | Expression(l), Compound(c, v) -> Expression( l @ [Compound(c, v)] )
      | Value(v), Expression(l) -> Expression( [Value(v)] @ l)
      | Compound(c, v), Expression(l) -> Expression( [Compound(c, v)] @ l)
    static member (+) (o:Operand, v:float) = o + Value(v)
    static member (+) (v:float, o:Operand) = Value(v) + o





  // let inline (+) (ops:Expression) (e:Operand) : Expression = List.append ops [e]

  // type Constraint = Constraint of Expression * lowerBound:float * upperBound:float

  // let inline (<==) (e:Expression) (ub:float)  = Constraint(e, Double.NegativeInfinity, ub)
  // let inline (>==) (e:Expression) (lb:float)  = Constraint(e, lb, Double.NegativeInfinity)
  // let inline (==) (e:Expression) (v:float)  = Constraint(e, v, v)

  (**************************************************************************************)

  // type VariableClass =
  //   | Boolean
  //   | Number

  // type Variable = {
  //   Name: string;
  //   Class: VariableClass;
  //   LowerBound: float;
  //   UpperBound: float;
  //   Value: float;
  // }
  // with
  //   static member Boolean name =
  //     { Name=name; Class=Boolean; LowerBound=0.0; UpperBound=1.0; Value=0.0 }
  //   static member Number name lowerBound upperBound =
  //     { Name=name; Class=Number; LowerBound=lowerBound; UpperBound=upperBound; Value=0.0 }
  //   member this.State =
  //     match this.Class with
  //     | Boolean -> this.Value > 0.5
  //     | _ -> failwith "Cannot get state on number variable"


  // type Operand =
  //   | Operand of float * Variable
  //   | Variable of Variable
  //   | Value of float
  //   | Add of Operand list
  //   static member (+) (o1:Operand, o2:Operand) =
  //       match o1,o2 with
  //       | Add(ops1), Add(ops2) -> Add(ops1 @ ops2)
  //       | Add(ops1), ops2 -> Add(ops1 @ [ ops2 ])
  //       | ops1, Add(ops2) -> Add(ops1 :: ops2)
  //       | ops1, ops2 -> Add [ ops1 ; ops2 ]
  //   static member (+) (op:Operand, num:float) = op + Value num
  //   static member (+) (num:float, op:Operand) = Value num + op


  (**************************************************************************************)


  // type Operand1 = {
  //   Coefficient: float
  //   Variable: Variable
  //   Value: float
  // }
  // with
  //   // static member (*) (var:Variable, coeff:float) =
  //   //   { Coefficient=coeff; Variable=var; Value=var.Value*coeff }
  //   static member (|*|) (coeff:float, var:Variable) =
  //     { Coefficient=coeff; Variable=var; Value=var.Value*coeff }
  //   // static member (+) (op1:Operand, op2:Operand) =
  //   //   {op1.Value}


  (**************************************************************************************)


  // type Variable =
  //     | Boolean of name:string * value:bool
  //     | Integer of name:string * lowerBound:int * upperBound:int * value:int
  //     | Real of name:string * lowerBound:float * upperBound:float * value:float

  // type Operand1 =
  //     | Real of float * Variable
  //     | Integer of int * Variable

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
