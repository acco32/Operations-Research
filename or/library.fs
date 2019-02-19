namespace Operations.Research


module LinearProgramming =
  open Operations.Research.Types

  let x = Variable.Boolean("choice A")
  let y = Variable.Real("choice B", 0.0, 2.0)
  let z = Variable.Integer("choice C", 0, 5)

  let sdf1 = 2 * x
  let sdf2 = -2.0 * x
  let sdf3 = -5 * y

  let expr = [] + 3*x + 1*z + 2*y + 6*x
  let con1 = expr <== 6.0
  let con2 = expr >== 6.0


