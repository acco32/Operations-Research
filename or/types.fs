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
      | (:? float as f), Number({Name=n; LowerBound=lb; UpperBound=ub; Value=v}) ->
          match f with
          | x when x <= ub && x >= lb ->
            Number({Name=n; LowerBound=lb; UpperBound=ub; Value=f})
          | _ -> invalidArg "s" "Out of Range"
      | (:? float), Boolean({Name=n; Value=v}) -> invalidArg "s" "Cannot set Boolean variable with Number value"
      | (:? bool), Number({Name=n; LowerBound=lb; UpperBound=ub; Value=v}) -> invalidArg "s" "Cannot set Number variable with Boolean value"
      | _ -> failwith "cannot set variable with unknown type"
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

  type Constraint = Constraint of Operand * lowerBound:float * upperBound:float

  type SolverParams = {
    Variables: Variable list
    Objective: Operand option
    Constraints: Constraint list
  }
  with
  static member Default =
    {Variables=List.Empty; Objective=None; Constraints=List.Empty}









