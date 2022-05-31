open Cast_cic
open Example

let delta i =
  let open Kernel.Ast in
  Lambda { id; dom = Unknown (i + 1); body = App (Var id, Var id) }

let omega i =
  let open Kernel.Ast in
  let d = delta i in
  App (d, d)

let elaborate = Elaboration.elaborate Reduction.reduce

let tests_base_elaborations () =
  let open Kernel.Ast in
  let idf : Kernel.Ast.term = Lambda { id; dom = Universe 1; body = Var id } in
  Alcotest.(check (pair Testable.term Testable.term))
    "Universe elaborates ok"
    (Universe 0, Universe 1)
    (elaborate empty_ctx (Universe 0) |> Result.get_ok);
  Alcotest.(check (pair Testable.term Testable.term))
    "lambda with universe elaborates"
    ( Lambda { id; dom = Universe 1; body = Var id }
    , Prod { id; dom = Universe 1; body = Universe 1 } )
    (elaborate empty_ctx idf |> Result.get_ok);
  Alcotest.(check (pair Testable.term Testable.term))
    "lambda with unknown elaborates"
    (let dom =
       Ast.Cast { source = unknown 1; target = Universe 0; term = Unknown (unknown 1) }
     in
     Lambda { id; dom; body = Var id }, Prod { id; dom; body = dom })
    (elaborate empty_ctx (Lambda { id; dom = Unknown 1; body = Var id }) |> Result.get_ok);
  Alcotest.(check (pair Testable.term Testable.term))
    "Prod with universe elaborates"
    (Prod { id; dom = Universe 1; body = Var id }, Universe 2)
    (elaborate empty_ctx (Prod { id; dom = Universe 1; body = Var id }) |> Result.get_ok);
  Alcotest.(check (pair Testable.term Testable.term))
    "Prod with unknown elaborates"
    (let dom =
       Ast.Cast { source = unknown 1; target = Universe 0; term = Unknown (unknown 1) }
     in
     ( Prod
         { id; dom; body = Cast { source = dom; target = Universe (-1); term = Var id } }
     , Universe 0 ))
    (elaborate empty_ctx (Prod { id; dom = Unknown 1; body = Var id }) |> Result.get_ok);
  Alcotest.(check (pair Testable.term Testable.term))
    "app without unknown"
    ( App
        ( Lambda { id; dom = Universe 1; body = Var id }
        , Cast { source = Universe 1; target = Universe 1; term = Universe 0 } )
    , Universe 1 )
    (elaborate empty_ctx (App (idf, Universe 0)) |> Result.get_ok);
  Alcotest.(check (pair Testable.term Testable.term))
    "app with unknown"
    ( App
        ( Cast
            { source = unknown 1; target = Reduction.(germ 1 HProd); term = Unknown (unknown 1) }
        , Cast { source = Universe 1; target = unknown 0; term = Universe 0 } )
    , unknown 0 )
    (elaborate empty_ctx (App (Unknown 1, Universe 0)) |> Result.get_ok)

let test_delta_elaborates () =
  Alcotest.(check (pair Testable.term Testable.term))
    "delta elaborates ok"
    ( Example.delta' 1
    , Prod
        { id
        ; dom =
            Cast { source = unknown 2; target = Universe 1; term = Unknown (unknown 2) }
        ; body = unknown (Config.cast_universe_level 1)
        } )
    (elaborate empty_ctx (delta 1) |> Result.get_ok)

let test_omega_elaborates () =
  Alcotest.(check (pair Testable.term Testable.term))
    "omega elaborates ok"
    (Example.omega 1, unknown (Config.cast_universe_level 1))
    (elaborate empty_ctx (omega 1) |> Result.get_ok)

(* The type for the elaborated term matches the inferred type *)
let correct_elaboration =
  QCheck.(
    Test.make ~count:1000 ~name:"correct elaboration" Arbitrary.gcic_term (fun t ->
        let elab_term = elaborate empty_ctx t in
        assume (elab_term |> Result.is_ok);
        let t', ty = Result.get_ok elab_term in
        match Typing.infer_type empty_ctx t' with
        | Ok ty' -> Ast.alpha_equal ty ty'
        | Error _ -> false))

let tests =
  [ "base elaborations", `Quick, tests_base_elaborations
  ; "delta 1 elaborates", `Quick, test_delta_elaborates
  ; "oemga 1 elaborates", `Quick, test_omega_elaborates
  ; QCheck_alcotest.to_alcotest correct_elaboration
  ]
