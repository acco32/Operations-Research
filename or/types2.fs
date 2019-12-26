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
      | Number({Name=n; Bounds=b; Value=Number.Integer(_)}), (:? int as i) -> Number({Name=n; Bounds=b;Value=Number.Integer(i)})
      | Number({Name=n; Bounds=b; Value=Number.Real(_)}), (:? float as f) -> Number({Name=n; Bounds=b;Value=Number.Real(f)})
      | Number({Name=n; Bounds=b; Value=Number.Real(_)}), (:? int as i) -> Number({Name=n; Bounds=b;Value=Number.Real(float(i))})
      | Number({Name=n; Bounds=b; Value=Number.Integer(_)}), (:? float as f) -> Number({Name=n; Bounds=b;Value=Number.Integer(int(f))})
      | _ -> failwith "Unable to set variable"


  type Operand2 =
    | Constant of Number
    | Variable of Variable2
    | CoefficientVariable of Number * Variable2

  type Expression =
    Operand2 list



