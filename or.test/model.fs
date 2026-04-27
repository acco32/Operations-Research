namespace Operations.Research.Test

open System
open TestTracks
open Operations.Research.Types
open Operations.Research.Models

module Models =

  let private throws (f: unit -> 'a) : bool =
    try
      f () |> ignore
      false
    with _ ->
      true

  let tests =
    suite
      "Models"
      [

        test "default model has empty fields" (fun () ->
          let mdl = Model.empty

          assertEqual 0 mdl.Variables.Length "no variables"
          |> combine (assertEqual None mdl.Objective "no objective")
          |> combine (assertEqual 0 mdl.Constraints.Length "no constraints")
          |> combine (assertEqual None mdl.Goal "no goal"))

        test "create model with decision variables" (fun () ->
          let x = Variable.boolean "a"
          let y = Variable.boolean "b"
          let mdl = Model.empty |> DecisionVars [ x; y ]
          assertEqual 2 mdl.Variables.Length "should hold 2 variables")

        test "create model with constraints" (fun () ->
          let x = Variable.real "a" 0.0 20.0
          let y = Variable.real "b" -4.0 100.0
          let c1 = 1.0 * x + (-3.4 * y) <== 4.5
          let c2 = 3.0 * x + 4.9 * y >== 50.0
          let c3 = 1.0 * x + 1.0 * y + 6.0 === 5.0
          let mdl = Model.empty |> Constraint c1 |> Constraint c2 |> Constraint c3
          assertEqual 3 mdl.Constraints.Length "should hold 3 constraints")

        test "create model with objective" (fun () ->
          let x = Variable.real "a" 0.0 20.0
          let y = Variable.real "b" -4.0 100.0
          let obj = 1.0 * x + 0.4 * y
          let mdl = Model.empty |> Objective obj
          assertEqual (Some obj) mdl.Objective "objective should be set")

        test "range operator with mixed integer and float bounds" (fun () ->
          let x = Variable.real "x" 0.0 10.0
          let c1 = 1.0 * x <-> (5, 10.5)
          let c2 = 1.0 * x <-> (5.5, 10)
          let c3 = 1.0 * x <-> (0.5, 2.5)
          let c4 = 1.0 * x <-> (5, 5)

          let isRange (con: Operations.Research.Types.Constraint) =
            match con.Kind with
            | Range _ -> true
            | _ -> false

          assertTrue (isRange c1) "c1 should be Range"
          |> combine (assertTrue (isRange c2) "c2 should be Range")
          |> combine (assertTrue (isRange c3) "c3 should be Range")
          |> combine (assertTrue (isRange c4) "c4 should be Range"))

        test "range operator with negative bounds" (fun () ->
          let x = Variable.real "x" -10.0 10.0
          let c = (1.0 * x) <-> (-5, -2)

          let isRange =
            match c.Kind with
            | Range _ -> true
            | _ -> false

          assertTrue isRange "negative range should produce Range constraint")

        test "range operator throws when lower exceeds upper" (fun () ->
          let x = Variable.real "x" 0.0 1.0
          assertTrue (throws (fun () -> (1.0 * x) <-> (10, 5))) "should throw on inverted range")

        test "constraint builder appends in insertion order" (fun () ->
          let x = Variable.real "x" 0.0 10.0
          let c1 = 1.0 * x <== 1.0
          let c2 = 1.0 * x <== 2.0
          let c3 = 1.0 * x <== 3.0
          let mdl = Model.empty |> Constraint c1 |> Constraint c2 |> Constraint c3

          let bounds =
            mdl.Constraints
            |> List.choose (fun c ->
              match c.Kind with
              | Range(_, Some hi) -> Some hi
              | _ -> None)

          assertEqual [ 1.0; 2.0; 3.0 ] bounds "constraints preserve insertion order") ]
