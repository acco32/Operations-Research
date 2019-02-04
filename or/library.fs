namespace Operations.Research
// open Google.OrTools.LinearSolver


module LinearProgramming =
  open Operations.Research.Types

  let Var (name: string) (lowerBound:obj) (upperBound:obj): Variable =
    match (lowerBound, upperBound) with
    | (:? int as x1), (:? int as x2) -> Variable.Integer (name, x1, x2)
    | (:? float as x1), (:? float as x2) -> Variable.Real (name, x1, x2)
    | (null, null) -> Variable.Boolean name
    | _ -> failwithf "Unknown variable type. Please check constructor."