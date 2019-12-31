namespace Operations.Research

module Types2 =
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

  type Interval =
    | Include
    | Exclude

  type NumberBounds = {
    Lower: Number
    Upper: Number
    Interval: Interval
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


  type VariableData = {
    Name:string
    Bounds: NumberBounds
    Value: Number
   }

  type Variable2 =
    | Boolean of VariableData
    | Number of VariableData
    static member Bool (name:string) =
      let bnd = {Lower=Number.Integer(0); Upper=Number.Integer(1); Interval=Interval.Include}
      Boolean({Name=name; Bounds=bnd; Value=Number.Integer(0)})
    static member Integer (name:string) =
      let bnd = {Lower=Number.Integer(0); Upper=Number.Integer(Int32.MaxValue); Interval=Interval.Include}
      Number({Name=name; Bounds=bnd; Value=Number.Integer(0)})
    static member Integer ((name:string), (lowerBound:int), (upperBound:int)) =
      let bnd = {Lower=Number.Integer(lowerBound); Upper=Number.Integer(upperBound); Interval=Interval.Include}
      Number({Name=name; Bounds=bnd; Value=Number.Integer(0)})
    static member Real (name:string) =
      let bnd = {Lower=Number.Real(0.0); Upper=Number.Real(Double.PositiveInfinity); Interval=Interval.Include}
      Number({Name=name; Bounds=bnd; Value=Number.Real(0.0)})
    static member Real ((name:string), (lowerBound:float), (upperBound:float)) =
      let bnd = {Lower=Number.Real(lowerBound); Upper=Number.Real(upperBound); Interval=Interval.Include}
      Number({Name=name; Bounds=bnd; Value=Number.Integer(0)})
    member this.Name =
      match this with
      | Number({Name=n; Bounds=_; Value=_}) -> n
      | Boolean({Name=n; Bounds=_; Value=_}) -> n
    member this.Data =
      match this with
      | Number({Name=_; Bounds=_; Value=v}) -> v
      | Boolean({Name=_; Bounds=_; Value=v}) -> v
    member this.State =
      match this with
      | Boolean({Name=_; Bounds=_; Value=v}) ->
          match v with
          | Integer(i) -> i.Equals(1)
          | Real(r) -> r.Equals(1.0)
      | Number(_) -> failwith "Only can be used for Boolean variables."
    member this.UpperBound =
      match this with
      | Number({Name=_; Bounds=bnd; Value=_}) -> bnd.Upper
      | Boolean({Name=_; Bounds=bnd; Value=_}) -> bnd.Upper
    member this.LowerBound =
      match this with
      | Number({Name=_; Bounds=bnd; Value=_}) -> bnd.Lower
      | Boolean({Name=_; Bounds=bnd; Value=_}) -> bnd.Lower
    member this.Set (s:obj) =
      match this, s with
      | Boolean({Name=n; Bounds=b; Value=Number.Integer(_)}), (:? bool as bl) ->
          match bl with
          | true -> Number({Name=n; Bounds=b;Value=Number.Integer(1)})
          | false -> Number({Name=n; Bounds=b;Value=Number.Integer(0)})
      | Boolean({Name=n; Bounds=b; Value=Number.Integer(_)}), (:? int as i) ->
          match i with
          | 1 -> Number({Name=n; Bounds=b;Value=Number.Integer(1)})
          | 0 -> Number({Name=n; Bounds=b;Value=Number.Integer(0)})
          | _ -> failwith "Only 1 or 0 are acceptable values"
      | Number({Name=n; Bounds=b; Value=Number.Integer(_)}), (:? int as i) ->
          match b.withinBounds(i) with
          | true ->  Number({Name=n; Bounds=b;Value=Number.Integer(i)})
          | false -> invalidArg "s" "Out of Range"
      | Number({Name=n; Bounds=b; Value=Number.Real(_)}), (:? float as f) ->
          match b.withinBounds(f) with
          | true ->  Number({Name=n; Bounds=b;Value=Number.Real(f)})
          | false -> invalidArg "s" "Out of Range"
      | Number({Name=n; Bounds=b; Value=Number.Real(_)}), (:? int as i) ->
          match b.withinBounds(i) with
          | true ->  Number({Name=n; Bounds=b;Value=Number.Real(float(i))})
          | false -> invalidArg "s" "Out of Range"
      | Number({Name=n; Bounds=b; Value=Number.Integer(_)}), (:? float as f) ->
          match b.withinBounds(f) with
          | true ->  Number({Name=n; Bounds=b;Value=Number.Integer(int(f))})
          | false -> invalidArg "s" "Out of Range"
      | _ -> failwith "Unable to set variable"
    static member (*) (c:float, v:Variable2) =
      let exp = Expression2.New()
      {
        exp with
          Statement = exp.Statement@[CoefficientArgument(Number.Real(c), v)];
          Variables = exp.Variables.Add(v.Name, v)
      }
    static member (*) (c:int, v:Variable2) =
      let exp = Expression2.New()
      {
        exp with
          Statement = exp.Statement@[CoefficientArgument(Number.Integer(c), v)];
          Variables = exp.Variables.Add(v.Name, v)
      }
    static member (+) ((arg1:Variable2), (arg2:Variable2)) : Expression2 =
      let exp = Expression2.New()
      {
        exp with
          Statement = exp.Statement@[Argument(arg1); Argument(arg2)];
          Variables = exp.Variables.Add(arg1.Name, arg1).Add(arg2.Name, arg2)
      }
    static member (+) ((arg1:Variable2), (arg2:int)) : Expression2 =
      let exp = Expression2.New()
      {
        exp with
          Statement = exp.Statement@[Argument(arg1); Constant(Number.Integer(arg2))];
          Variables = exp.Variables.Add(arg1.Name, arg1).Add(arg2.ToString(), Variable2.Integer(arg2.ToString(), arg2, arg2))
      }
    static member (+) ((arg1:Variable2), (arg2:float)) : Expression2 =
      let exp = Expression2.New()
      {
        exp with
          Statement = exp.Statement@[Argument(arg1); Constant(Number.Real(arg2))];
          Variables = exp.Variables.Add(arg1.Name, arg1).Add(arg2.ToString(), Variable2.Real(arg2.ToString(), arg2, arg2))
      }
    static member (+) ((arg1:Variable2), (arg2:Expression2)) : Expression2 =
      {
        arg2 with
          Statement = [Argument(arg1)]@arg2.Statement;
          Variables = arg2.Variables.Add(arg1.Name, arg1)
      }
    static member (+) ((arg1:Expression2), (arg2:Variable2)) : Expression2 =
      {
        arg1 with
          Statement = arg1.Statement@[Argument(arg2)]
          Variables = arg1.Variables.Add(arg2.Name, arg2)
      }
  and Operand2 =
    | Constant of Number
    | Argument of Variable2
    | CoefficientArgument of Number * Variable2
  and Expression2 = {
    Statement: Operand2 list
    Variables: Map<string, Variable2>
  }
  with
  static member New() = {Statement=List.empty; Variables=Map.empty}
  static member Eval (vars:Set<Variable2>) (exp:Expression2): Number =
    if vars.IsEmpty then invalidArg "vars" "Set cannot be empty"

    let tmp = List.map (fun (v:Variable2) ->  exp.Variables.ContainsKey(v.Name) ) (vars |> Set.toList)
    let allVariablesPresent = tmp |> List.reduce (&&)
    if not allVariablesPresent then invalidArg "vars" "one (or many) input variable names do not match expression variable names"

    let ans = (exp.Statement) |> List.map ( fun (v:Operand2) ->
      match v with
      | Constant(c) -> c.toInt
      | Argument(a) ->
          let ap = List.find (fun (f:Variable2) -> f.Name.Equals(a.Name)) (vars |> Set.toList)
          ap.Data.toInt
      | CoefficientArgument(c,a) ->
          let ap = List.find (fun (f:Variable2) -> f.Name.Equals(a.Name)) (vars |> Set.toList)
          c.toInt * ap.Data.toInt
    )

    Number.Integer(ans |> List.reduce (+))
  static member (+) ((arg1:Expression2), (arg2:Expression2)) : Expression2 =
    {
      arg1 with
        Statement = arg1.Statement@arg2.Statement
        Variables = Map.fold (fun acc key value -> Map.add key value acc) arg1.Variables arg2.Variables
    }
  static member (+) ((arg1:Expression2), (arg2:int)) : Expression2 =
    {
      arg1 with
        Statement = arg1.Statement@[Constant(Number.Integer(arg2))]
        Variables = arg1.Variables.Add(arg2.ToString(), Variable2.Integer(arg2.ToString(), arg2, arg2))
    }
  static member (+) ((arg1:Expression2), (arg2:float)) : Expression2 =
    {
      arg1 with
        Statement = arg1.Statement@[Constant(Number.Real(arg2))]
        Variables = arg1.Variables.Add(arg2.ToString(), Variable2.Real(arg2.ToString(), arg2, arg2))
    }
