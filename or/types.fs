namespace Operations.Research

module Types =

  open System

  let private addCoeffs (a: Map<string, float>) (b: Map<string, float>) : Map<string, float> =
    Map.fold
      (fun acc name c ->
        let total = (Map.tryFind name acc |> Option.defaultValue 0.0) + c

        if total = 0.0 then
          Map.remove name acc
        else
          Map.add name total acc)
      a
      b

  let private scaleCoeffs (s: float) (m: Map<string, float>) : Map<string, float> =
    if s = 0.0 then
      Map.empty
    else
      m |> Map.map (fun _ c -> s * c)

  type Kind =
    | Boolean
    | Integer
    | Real

  type Variable =
    { Name: string
      Kind: Kind
      Lower: float option
      Upper: float option }

    static member (*)(s: float, v: Variable) : LinearExpression =
      if s = 0.0 then
        { Coefficients = Map.empty
          Constant = 0.0 }
      else
        { Coefficients = Map.ofList [ v.Name, s ]
          Constant = 0.0 }

    static member (*)(v: Variable, s: float) : LinearExpression = s * v
    static member (*)(s: int, v: Variable) : LinearExpression = float s * v
    static member (*)(v: Variable, s: int) : LinearExpression = float s * v

    static member (~-)(v: Variable) : LinearExpression = -1.0 * v

    static member (+)(a: Variable, b: Variable) : LinearExpression = (1.0 * a) + (1.0 * b)
    static member (+)(a: Variable, b: float) : LinearExpression = (1.0 * a) + b
    static member (+)(a: float, b: Variable) : LinearExpression = a + (1.0 * b)
    static member (+)(a: Variable, b: int) : LinearExpression = (1.0 * a) + float b
    static member (+)(a: int, b: Variable) : LinearExpression = float a + (1.0 * b)

    static member (-)(a: Variable, b: Variable) : LinearExpression = (1.0 * a) - (1.0 * b)
    static member (-)(a: Variable, b: float) : LinearExpression = (1.0 * a) - b
    static member (-)(a: float, b: Variable) : LinearExpression = a - (1.0 * b)
    static member (-)(a: Variable, b: int) : LinearExpression = (1.0 * a) - float b
    static member (-)(a: int, b: Variable) : LinearExpression = float a - (1.0 * b)

  and LinearExpression =
    { Coefficients: Map<string, float>
      Constant: float }

    static member (+)(a: LinearExpression, b: LinearExpression) : LinearExpression =
      { Coefficients = addCoeffs a.Coefficients b.Coefficients
        Constant = a.Constant + b.Constant }

    static member (+)(a: LinearExpression, b: float) : LinearExpression = { a with Constant = a.Constant + b }
    static member (+)(a: float, b: LinearExpression) : LinearExpression = b + a
    static member (+)(a: LinearExpression, b: int) : LinearExpression = a + float b
    static member (+)(a: int, b: LinearExpression) : LinearExpression = float a + b
    static member (+)(a: LinearExpression, b: Variable) : LinearExpression = a + (1.0 * b)
    static member (+)(a: Variable, b: LinearExpression) : LinearExpression = (1.0 * a) + b

    static member (~-)(e: LinearExpression) : LinearExpression =
      { Coefficients = e.Coefficients |> Map.map (fun _ c -> -c)
        Constant = -e.Constant }

    static member (-)(a: LinearExpression, b: LinearExpression) : LinearExpression = a + (-b)
    static member (-)(a: LinearExpression, b: float) : LinearExpression = a + (-b)
    static member (-)(a: float, b: LinearExpression) : LinearExpression = a + (-b)
    static member (-)(a: LinearExpression, b: int) : LinearExpression = a + float (-b)
    static member (-)(a: int, b: LinearExpression) : LinearExpression = float a + (-b)
    static member (-)(a: LinearExpression, b: Variable) : LinearExpression = a + (-(1.0 * b))
    static member (-)(a: Variable, b: LinearExpression) : LinearExpression = (1.0 * a) + (-b)

    static member (*)(s: float, e: LinearExpression) : LinearExpression =
      { Coefficients = scaleCoeffs s e.Coefficients
        Constant = s * e.Constant }

    static member (*)(e: LinearExpression, s: float) : LinearExpression = s * e
    static member (*)(s: int, e: LinearExpression) : LinearExpression = float s * e
    static member (*)(e: LinearExpression, s: int) : LinearExpression = float s * e

  type ConstraintKind =
    | Range of lower: float option * upper: float option
    | NotEqual of value: float

  type Constraint =
    { Name: string option
      Expression: LinearExpression
      Kind: ConstraintKind }

  type Goal =
    | Maximize
    | Minimize

  type Model =
    { Variables: Variable list
      Objective: LinearExpression option
      Constraints: Constraint list
      Goal: Goal option }

    static member empty: Model =
      { Variables = []
        Objective = None
        Constraints = []
        Goal = None }

  type Status =
    | Optimal
    | Feasible
    | Infeasible
    | Unbounded
    | NotSolved

  type Solution =
    { Status: Status
      Objective: float option
      Values: Map<string, float> }

  module Variable =

    let private cleanFloat (x: float) =
      if Double.IsInfinity x || Double.IsNaN x then
        None
      else
        Some x

    let private cleanInt (x: int) =
      if x = Int32.MinValue || x = Int32.MaxValue then
        None
      else
        Some(float x)

    /// Boolean variable, domain {0, 1}.
    let boolean (name: string) : Variable =
      { Name = name
        Kind = Boolean
        Lower = Some 0.0
        Upper = Some 1.0 }

    /// Real-valued variable. Infinities on either side become unbounded (None).
    let real (name: string) (lower: float) (upper: float) : Variable =
      { Name = name
        Kind = Real
        Lower = cleanFloat lower
        Upper = cleanFloat upper }

    /// Real-valued variable, fully unbounded.
    let realFree (name: string) : Variable =
      { Name = name
        Kind = Real
        Lower = None
        Upper = None }

    /// Integer-valued variable. Int32 min/max sentinels treated as unbounded.
    let integer (name: string) (lower: int) (upper: int) : Variable =
      { Name = name
        Kind = Integer
        Lower = cleanInt lower
        Upper = cleanInt upper }

    /// Integer-valued variable, fully unbounded.
    let integerFree (name: string) : Variable =
      { Name = name
        Kind = Integer
        Lower = None
        Upper = None }


  module LinearExpression =

    let zero: LinearExpression =
      { Coefficients = Map.empty
        Constant = 0.0 }

    let ofConstant (c: float) : LinearExpression =
      { Coefficients = Map.empty
        Constant = c }

    let ofVariable (v: Variable) : LinearExpression = 1.0 * v

    /// Evaluate at given variable values. Missing variables treated as 0.
    let evaluate (values: Map<string, float>) (e: LinearExpression) : float =
      e.Coefficients
      |> Map.fold
        (fun acc name coeff ->
          let v = Map.tryFind name values |> Option.defaultValue 0.0
          acc + coeff * v)
        e.Constant

    let inline sum (items: ^a seq) : LinearExpression =
        items |> Seq.fold (fun (acc: LinearExpression) x -> acc + x) zero