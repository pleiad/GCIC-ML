open Cast_cic

let name_of_int n = string_of_int n |> Ast.Name.of_string
let id = Ast.Name.of_string "x"

let strong_normalization =
  QCheck.(
    Test.make ~count:1000 ~name:"strong normalization" Arbitrary.term (fun t ->
        assume (Typing.infer_type Context.empty t |> Result.is_ok);
        Reduction.reduce t |> Ast.is_canonical))

let subject_reduction_empty_ctx =
  let ctx = Context.empty in
  QCheck.(
    Test.make ~count:1000 ~name:"subject reduction in empty ctx" Arbitrary.term
      (fun t ->
        let ty = Typing.infer_type ctx t in
        assume (Result.is_ok ty);
        let t' = Reduction.step ctx t in
        assume (Result.is_ok t');
        Typing.check_type ctx (Result.get_ok t') (Result.get_ok ty)
        |> Result.is_ok))

let progress_empty_ctx =
  let ctx = Context.empty in
  QCheck.(
    Test.make ~count:1000 ~name:"progress in empty ctx" Arbitrary.term (fun t ->
        assume (Typing.infer_type ctx t |> Result.is_ok);
        Ast.is_canonical t || Reduction.step ctx t |> Result.is_ok))

let test_app_reduce () =
  let open Ast in
  Alcotest.check Testable.term "beta identity" (Universe 0)
    (Reduction.reduce
       (App (Lambda { id; dom = Universe 1; body = Var id }, Universe 0)))

let test_unknown_reduce () =
  let open Ast in
  Alcotest.check Testable.term "Prod-Unk universe"
    (Lambda { id; dom = Universe 5; body = Unknown (Universe 0) })
    (Reduction.reduce
       (Unknown (Prod { id; dom = Universe 5; body = Universe 0 })));
  Alcotest.check Testable.term "Down-Unk universe" (Unknown (Universe 0))
    (Reduction.reduce
       (Cast
          {
            source = Unknown (Universe 0);
            target = Universe 0;
            term = Unknown (Unknown (Universe 1));
          }))

let test_error_reduce () =
  let open Ast in
  Alcotest.check Testable.term "Prod-Err universe"
    (Lambda { id; dom = Universe 5; body = Err (Universe 0) })
    (Reduction.reduce (Err (Prod { id; dom = Universe 5; body = Universe 0 })));
  Alcotest.check Testable.term "Down-Err universe" (Err (Universe 0))
    (Reduction.reduce
       (Cast
          {
            source = Unknown (Universe 0);
            target = Universe 0;
            term = Err (Unknown (Universe 1));
          }))

let tests =
  [
    ("reduce unknown", `Quick, test_unknown_reduce);
    ("reduce error", `Quick, test_error_reduce);
    ("reduce app", `Quick, test_app_reduce);
    QCheck_alcotest.to_alcotest strong_normalization;
    QCheck_alcotest.to_alcotest subject_reduction_empty_ctx;
    QCheck_alcotest.to_alcotest progress_empty_ctx;
  ]
