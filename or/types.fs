namespace Operations.Research

module Types =
  open System

  type BooleanVariableData = {
    Name: string;
    Value: bool;
   }

   type NumberVariableData = {
     Name:string;
     LowerBound:float;
     UpperBound:float;
     Value: float;
   }

  type Variable =
    | Boolean of BooleanVariableData
    | Number of NumberVariableData
    static member Bool (name:string) =
      Boolean({Name=name; Value=false})
    static member Num (name:string) (lowerBound:float) (upperBound:float) =
      Number({Name=name; LowerBound=lowerBound; UpperBound=upperBound; Value=0.0})
    static member (*) (c:float, v:Variable) = Compound(c, v)
    static member Set (s:obj) (var:Variable) =
      match s, var with
      | (:? bool as b), Boolean({Name=n; Value=v}) -> Boolean({Name=n; Value=b})
      | (:? float as f), Number({Name=n; LowerBound=lb; UpperBound=ub; Value=v}) -> Number({Name=n; LowerBound=lb; UpperBound=ub; Value=f})
      | (:? float), Boolean({Name=n; Value=v}) -> failwith "Cannot set Boolean variable with Number value"
      | (:? bool), Number({Name=n; LowerBound=lb; UpperBound=ub; Value=v}) -> failwith "Cannot set Number variable with Boolean value"
      | _ -> failwith "cannot set variable with unknown type."
    member this.BoolData() =
      match this with
      | Boolean({Name=n; Value=v}) ->  {Name=n; Value=v}
      | _ -> failwith "Cannot retrive boolean data"
    member this.NumberData() =
      match this with
      | Number({Name=n; LowerBound=lb; UpperBound=ub; Value=v}) -> {Name=n; LowerBound=lb; UpperBound=ub; Value=v}
      | _ -> failwith "Cannot retrive number data"
  and Operand =
    | Compound of float * Variable
    | Value of float
    | Expression of Operand list
    static member (+) (o1:Operand, o2:Operand) =
      match o1,o2 with
      | Expression(a), Expression(b) -> Expression( a @ b )
      | Expression(l), Value(v) -> Expression( l @ [Value(v)]  )
      | Expression(l), Compound(c, v) -> Expression( l @ [Compound(c, v)] )
      | Value(v), Expression(l) -> Expression( [Value(v)] @ l)
      | Compound(c, v), Expression(l) -> Expression( [Compound(c, v)] @ l)
      | Compound(c1, v1), Compound(c2,v2) -> Expression( [Compound(c1, v1); Compound(c2, v2)])
    static member (+) (o:Operand, v:float) = o + Value(v)
    static member (+) (v:float, o:Operand) = Value(v) + o


  // type SolverParams = {
  //   Name: string
  //   Variables: Variable list
  //   Objective: string
  //   Constraints: Constraint list
  // }


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
