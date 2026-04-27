namespace Operations.Research.Test

open System
open TestTracks
open Operations.Research.Types

module BasicTypes =

  let tests =
    suite
      "Basic Types"
      [

        test "create boolean variable" (fun () ->
          let v = Variable.boolean "bool"

          assertEqual "bool" v.Name "name"
          |> combine (assertEqual Boolean v.Kind "kind")
          |> combine (assertEqual (Some 0.0) v.Lower "lower bound")
          |> combine (assertEqual (Some 1.0) v.Upper "upper bound"))

        test "create real variable with bounds" (fun () ->
          let v = Variable.real "real" -1.0 2.0

          assertEqual "real" v.Name "name"
          |> combine (assertEqual Real v.Kind "kind")
          |> combine (assertEqual (Some -1.0) v.Lower "lower bound")
          |> combine (assertEqual (Some 2.0) v.Upper "upper bound"))

        test "create integer variable with bounds" (fun () ->
          let v = Variable.integer "int" 0 10

          assertEqual "int" v.Name "name"
          |> combine (assertEqual Integer v.Kind "kind")
          |> combine (assertEqual (Some 0.0) v.Lower "lower bound")
          |> combine (assertEqual (Some 10.0) v.Upper "upper bound"))

        test "real variable with infinity bounds becomes unbounded" (fun () ->
          let v = Variable.real "x" Double.NegativeInfinity Double.PositiveInfinity

          assertEqual None v.Lower "lower should be None"
          |> combine (assertEqual None v.Upper "upper should be None"))

        test "realFree gives unbounded variable" (fun () ->
          let v = Variable.realFree "x"

          assertEqual None v.Lower "lower"
          |> combine (assertEqual None v.Upper "upper")
          |> combine (assertEqual Real v.Kind "kind"))

        test "integerFree gives unbounded variable" (fun () ->
          let v = Variable.integerFree "x"

          assertEqual None v.Lower "lower"
          |> combine (assertEqual None v.Upper "upper")
          |> combine (assertEqual Integer v.Kind "kind"))

        test "integer variable with Int32 sentinels becomes unbounded" (fun () ->
          let v = Variable.integer "x" Int32.MinValue Int32.MaxValue

          assertEqual None v.Lower "lower should be None"
          |> combine (assertEqual None v.Upper "upper should be None"))

        test "scalar times variable creates expression with one term" (fun () ->
          let x = Variable.real "a" 0.0 1.0
          let result = 1 * x

          assertEqual 1 result.Coefficients.Count "one coefficient"
          |> combine (assertEqual 0.0 result.Constant "no constant")
          |> combine (assertEqual 1.0 result.Coefficients.["a"] "coefficient is 1.0"))

        test "add two variables creates expression with two terms" (fun () ->
          let x = Variable.integerFree "a"
          let y = Variable.integerFree "b"
          let result = x + y

          assertEqual 2 result.Coefficients.Count "two coefficients"
          |> combine (assertEqual 0.0 result.Constant "no constant"))

        test "add variable and constant creates expression with constant" (fun () ->
          let x = Variable.integerFree "a"
          let result = x + 77

          assertEqual 1 result.Coefficients.Count "one coefficient"
          |> combine (assertEqual 77.0 result.Constant "constant is 77"))

        test "add same variable twice combines coefficients" (fun () ->
          let x = Variable.realFree "a"
          let result = x + x

          assertEqual 1 result.Coefficients.Count "should collapse to one term"
          |> combine (assertEqual 2.0 result.Coefficients.["a"] "coefficient should be 2.0"))

        test "subtract variable from itself zeroes the coefficient" (fun () ->
          let x = Variable.realFree "a"
          let result = x - x

          assertEqual 0 result.Coefficients.Count "zero coefficients should be dropped"
          |> combine (assertEqual 0.0 result.Constant "no constant"))

        test "multi-variable expression preserves all terms and constant" (fun () ->
          let x = Variable.integer "a" 0 10
          let y = Variable.integer "b" 0 10
          let z = Variable.integer "c" 0 10
          let result = x + 2 * y + 5 * z + 80

          assertEqual 3 result.Coefficients.Count "three coefficients"
          |> combine (assertEqual 80.0 result.Constant "constant is 80")
          |> combine (assertEqual 1.0 result.Coefficients.["a"] "a coefficient")
          |> combine (assertEqual 2.0 result.Coefficients.["b"] "b coefficient")
          |> combine (assertEqual 5.0 result.Coefficients.["c"] "c coefficient"))

        test "evaluate expression at given variable values" (fun () ->
          let x = Variable.integerFree "a"
          let y = Variable.integerFree "b"
          let expr = x + 2 * y + 10
          let values = Map.ofList [ "a", 5.0; "b", 5.0 ]
          let result = LinearExpression.evaluate values expr
          assertEqual 25.0 result "5 + 2*5 + 10 = 25")

        test "evaluate treats missing variables as zero" (fun () ->
          let x = Variable.integerFree "a"
          let y = Variable.integerFree "b"
          let expr = x + 2 * y + 10
          let result = LinearExpression.evaluate (Map.ofList [ "a", 5.0 ]) expr
          assertEqual 15.0 result "5 + 2*0 + 10 = 15") ]
