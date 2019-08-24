namespace Operations.Research

module Types =
  open System

  type Number =
    | Integer of int
    | Real of float
    member this.toInt : int =
      match this with
      | Integer(i) -> i
      | Real (i) -> int(i)
    member this.toFloat : float =
      match this with
      | Integer(f) -> float(f)
      | Real (f) -> f


  type NumberBounds = {
    Lower: Number
    Upper: Number
  }
  with
  member this.withinBounds (x:obj): bool =
    match x with
    | (:? int as i) ->
        i <= this.Upper.toInt && i >= this.Lower.toInt
    | (:? float as f) ->
        f <= this.Upper.toFloat && f >= this.Lower.toFloat
    | _ -> failwith "Can only parse integer or float values"
  end

  type VariableDataValue =
    | Boolean of bool
    | Integer of Number
    | Real of Number
    member this.Number =
      match this with
      | Real(n)| Integer(n) -> n
      | Boolean(_) -> failwith "Cannot get value"
    member this.Selected =
      match this with
      | Boolean(b) -> b
      | Real(_)| Integer(_) -> failwith "Cannot get value"

  type BooleanVariableData = {
    Name: string;
    Value: VariableDataValue;
   }

  type NumberVariableData = {
    Name:string
    Bounds: NumberBounds
    Value: VariableDataValue
   }

  type Variable =
    | Boolean of BooleanVariableData
    | Number of NumberVariableData
    static member Bool (name:string) =
      Boolean({Name=name; Value=VariableDataValue.Boolean(false)})
    static member Integer (name:string) =
      let bnd = {Lower=Number.Integer(0); Upper=Number.Integer(int(Int64.MaxValue))}
      Number({Name=name; Bounds=bnd; Value=VariableDataValue.Integer(Number.Integer(0))})
    static member Integer ((name:string), (lowerBound:int), (upperBound:int)) =
      let bnd = {Lower=Number.Integer(lowerBound); Upper=Number.Integer(upperBound)}
      Number({Name=name; Bounds=bnd; Value=VariableDataValue.Integer(Number.Integer(0))})
    static member Real (name:string) =
      let bnd = {Lower=Number.Real(0.0); Upper=Number.Real(Double.PositiveInfinity)}
      Number({Name=name; Bounds=bnd; Value=VariableDataValue.Real(Number.Real(0.0))})
    static member Real ((name:string), (lowerBound:float), (upperBound:float)) =
      let bnd = {Lower=Number.Real(lowerBound); Upper=Number.Real(upperBound)}
      Number({Name=name; Bounds=bnd; Value=VariableDataValue.Real(Number.Integer(0))})
    member this.Name =
      match this with
      | Boolean({Name=n; Value=_}) ->  n
      | Number({Name=n; Bounds=_; Value=_}) -> n
    member this.UpperBound =
      match this with
      | Number({Name=_; Bounds=bnd; Value=_}) -> bnd.Upper
      | _ -> failwith "can only retrieve bounds for numeric variables"
    member this.LowerBound =
      match this with
      | Number({Name=_; Bounds=bnd; Value=_}) -> bnd.Lower
      | _ -> failwith "can only retrieve bounds for numeric variables"
    static member Set (s:obj) (var:Variable) =
      match s, var with
      | (:? bool as b), Boolean({Name=n; Value=_}) -> Boolean({Name=n; Value=VariableDataValue.Boolean(b)})
      | (:? float as f), Number({Name=n; Bounds=b; Value=VariableDataValue.Real(_)}) ->
          match b.withinBounds(f) with
          | true -> Number({Name=n; Bounds=b; Value=VariableDataValue.Real(Number.Real(f))})
          | false -> invalidArg "s" "Out of Range"
      | (:? int as i), Number({Name=n; Bounds=b; Value=VariableDataValue.Integer(_)}) ->
          match b.withinBounds(i) with
          | true -> Number({Name=n; Bounds=b; Value=VariableDataValue.Real(Number.Integer(i))})
          | false -> invalidArg "s" "Out of Range"
      | (:? int), Number({Name=_; Bounds=_; Value=VariableDataValue.Real(_)}) -> invalidArg "s" "Cannot set Integer variable with Real value"
      | (:? float), Number({Name=_; Bounds=_; Value=VariableDataValue.Integer(_)}) -> invalidArg "s" "Cannot set Real variable with Integer value"
      | (:? float), Boolean(_) -> invalidArg "s" "Cannot set Boolean variable with Number value"
      | (:? bool), Number(_) -> invalidArg "s" "Cannot set Number variable with Boolean value"
      | _ -> failwith "cannot set variable with unknown type"
    member this.Data =
      match this with
      | Boolean({Name=_; Value=v}) | Number({Name=_; Bounds=_; Value=v}) -> v
    static member (*) (c:float, v:Variable) = Compound(Number.Real(c), v)
    static member (*) (v:Variable, c:float) = Compound(Number.Real(c), v)
    static member (*) (c:int, v:Variable) = Compound(Number.Integer(c), v)
    static member (*) (v:Variable, c:int) = Compound(Number.Integer(c), v)

  and Operand =
    | Compound of Number * Variable
    | Value of Number
    | Expression of Operand list
    static member (+) (o1:Operand, o2:Operand) =
      match o1,o2 with
      | Expression(a), Expression(b) -> Expression( a @ b )
      | Expression(l), Value(v) -> Expression( l @ [Value(v)]  )
      | Expression(l), Compound(c, v) -> Expression( l @ [Compound(c, v)] )
      | Value(v), Expression(l) -> Expression( [Value(v)] @ l)
      | Compound(c, v), Expression(l) -> Expression( [Compound(c, v)] @ l)
      | Compound(c1, v1), Compound(c2,v2) -> Expression( [Compound(c1, v1); Compound(c2, v2)])
      | Compound(c, v), Value(vl) -> Expression( [Compound(c, v); Value(vl)])
      | Value(v), Compound(c1, v1) -> Expression( [Value(v); Compound(c1, v1)])
      | _,_ -> failwith "cannot match operand"
    static member (+) (o:Operand, v:float) = o + Value(Number.Real(v))
    static member (+) (v:float, o:Operand) = Value(Number.Real(v)) + o
    static member (+) (o:Operand, v:int) = o + Value(Number.Integer(v))
    static member (+) (v:int, o:Operand) = Value(Number.Integer(v)) + o


  type Constraint = Constraint of Operand * bounds:NumberBounds

  type Goal =
    /// Goal is unset
    | Unset
    /// Maximize the Objective Function
    | Maximize
    /// Minimize the Objective Function
    | Minimize

  type Model = {
    Variables: Variable list
    Objective: Operand option
    Constraints: Constraint list
    Goal: Goal
  }
  with
  static member Default =
    {Variables=List.Empty; Objective=None; Constraints=List.Empty; Goal=Goal.Unset}
  end

  type SolverSolution = {
    Objective: Number;
    Variables: Map<string, Variable>;
    Optimal: bool;
  }

  type SolverError = {
    Code: int;
    Message: string
  }

  type SolverResult =
    | Solution of SolverSolution
    | Error of SolverError
    member this.Sol : SolverSolution =
      match this with
      | Solution(s) -> s
      | _ -> failwith "Unknown error occured when retrieving solution"
    member this.Err : SolverError =
      match this with
      | Error(e) -> e
      | _ -> failwith "Unknown error occured when retrieving error information"








