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



  /// Operand in expression
  type Term = {
    Name: string
    Coefficient: Number
    Bounds: NumberBounds
    Exponent: Number
    Value: Number
    IsBoolean: bool
  }


  type Expression = {
    Terms: Term list
  }
  with
  static member (*) (a:obj, b:obj) =
    match a, b with
    | (:? int as s), (:? Expression as t) when t.Terms.Length = 1 -> { Terms=[{ t.Terms.[0] with Coefficient=Number.Integer(s) }] }
    | (:? float as s), (:? Expression as t) when t.Terms.Length = 1 -> { Terms=[{ t.Terms.[0] with Coefficient=Number.Real(s) }] }
    | (:? Expression as t), (:? int as s) when t.Terms.Length = 1 -> { Terms=[{ t.Terms.[0] with Coefficient=Number.Integer(s) }] }
    | (:? Expression as t), (:? float as s) when t.Terms.Length = 1 -> { Terms=[{ t.Terms.[0] with Coefficient=Number.Real(s) }] }
    | _ -> failwith "Cannot use (*) operator on expression"
  static member (+) (a:obj, b:obj) =
    match a, b with
    | (:? int as s), (:? Expression as t) ->
        {Terms=[{ Name = sprintf "%d" s; Coefficient = Number.Integer(1); Exponent = Number.Integer(1); Bounds = { Lower=Integer(s); Upper=Integer(s); Interval=Include }; Value = Number.Integer(s); IsBoolean=false}]@t.Terms }
    | (:? Expression as t), (:? int as s) ->
        {Terms=t.Terms@[{ Name = sprintf "%d" s; Coefficient = Number.Integer(1); Exponent = Number.Integer(1); Bounds = { Lower=Integer(s); Upper=Integer(s); Interval=Include }; Value = Number.Integer(s); IsBoolean=false}]}
    | (:? float as s), (:? Expression as t) ->
        {Terms=[{ Name = sprintf "%f" s; Coefficient = Number.Integer(1); Exponent = Number.Integer(1); Bounds = { Lower=Real(s); Upper=Real(s); Interval=Include }; Value = Number.Real(s); IsBoolean=false}]@t.Terms}
    | (:? Expression as t), (:? float as s) ->
        {Terms=t.Terms@[{ Name = sprintf "%f" s; Coefficient = Number.Integer(1); Exponent = Number.Integer(1); Bounds = { Lower=Real(s); Upper=Real(s); Interval=Include }; Value = Number.Real(s); IsBoolean=false}]}
    | (:? Expression as s), (:? Expression as t) ->
        {Terms=s.Terms@t.Terms}
    | _ -> failwith "Cannot use (+) operator on expression"

  end

  /// Term in model that can change
  module Variable =
    /// Boolean Variable
    /// Special Term with only acceptable values being {0, 1}
    let boolean (name:string) : Term =
      {
        Name = name;
        Coefficient = Number.Integer(1);
        Exponent = Number.Integer(1);
        Bounds = { Lower=Integer(0); Upper=Integer(1); Interval=Include };
        Value = Number.Integer(0);
        IsBoolean = true
      }

    /// Variable in the domain of real numbers
    let real (name:string) (lowerBound:float) (upperBound:float) : Term =
      {
        Name = name;
        Coefficient = Number.Real(1.0);
        Exponent = Number.Real(1.0);
        Bounds = { Lower=Real(lowerBound); Upper=Real(upperBound); Interval=Include };
        Value = Number.Real(Double.NegativeInfinity);
        IsBoolean = false
      }

    /// named variable with default bounds being the integer minimum and maximum of OS platform
    let realDefault (name:string) : Term =
      real name Double.PositiveInfinity Double.PositiveInfinity

    /// Variable in the domain of integer numbers
    let integer (name:string) (lowerBound:int) (upperBound:int) : Term =
      {
        Name = name;
        Coefficient = Number.Real(1.0);
        Exponent = Number.Real(1.0);
        Bounds = { Lower=Integer(lowerBound); Upper=Integer(upperBound); Interval=Include };
        Value = Number.Integer(Int32.MinValue);
        IsBoolean = false
      }

    /// named variable with default bounds being the integer minimum and maximum of OS platform
    let integerDefault (name:string) : Term =
      integer name Int32.MinValue Int32.MaxValue

    /// sets the value of the term
    let set (value:obj) (t:Term) : Term =
      match value with
      | :? bool as b  when t.IsBoolean = true -> {t with Value = Number.Integer(1)}
      | :? bool as b  when t.IsBoolean = false -> {t with Value = Number.Integer(0)}
      | :? int as i  when t.IsBoolean = true -> invalidArg "value" "cannot set boolean variable with input"
      | :? float as f  when t.IsBoolean = true -> invalidArg "value" "cannot set boolean variable with input"
      | :? int as i -> {t with Value = Number.Integer(i)}
      | :? float as f -> {t with Value = Number.Real(f)}
      |_ -> invalidArg "t" "Out of Range"

    /// The state of a boolean variable. All other values return an error.
    let state (t:Term): bool =
      match t.IsBoolean with
      | true  when t.Value.toInt.Equals(1) -> true
      | true  when t.Value.toInt.Equals(0) -> false
      | _ -> failwith "state function only can be used for Boolean variables"



  /// Converts term to expression to be used in equations
  let toExpression (t:Term) : Expression = {Terms=[t]}

  let eval (exp:Expression) : Number =
    let a = List.map (fun term -> term.Coefficient.toFloat * Math.Pow(term.Value.toFloat, term.Exponent.toFloat)) exp.Terms
    let result = List.reduce (+) a
    Number.Real(result)


  type Constraint = Constraint of Expression * bounds:NumberBounds

  type Goal =
    /// Goal is unset
    | Unset
    /// Maximize the Objective Function
    | Maximize
    /// Minimize the Objective Function
    | Minimize

  type Model = {
    Variables: Variable list
    Objective: Expression option
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

