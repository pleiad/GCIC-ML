open Kernel
open Arbitrary

let name_of_int n = string_of_int n |> Cast_cic.Name.of_string
let id = Cast_cic.Name.of_string "x"

let strong_normalization =
  QCheck.(
    Test.make ~count:1000 ~name:"strong normalization" arbitrary_cast_cic_term
      (fun t ->
        assume (Typing.infer_type Context.empty t |> Result.is_ok);
        Reduction.reduce t |> Cast_cic.is_canonical))

let subject_reduction_empty_ctx =
  let ctx = Context.empty in
  QCheck.(
    Test.make ~count:1000 ~name:"subject reduction in empty ctx" arbitrary_cast_cic_term
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
    Test.make ~count:1000 ~name:"progress in empty ctx" arbitrary_cast_cic_term
      (fun t ->
        assume (Typing.infer_type ctx t |> Result.is_ok);
        (Cast_cic.is_canonical t || (Reduction.step ctx t |> Result.is_ok))
        ))

(* TODO: Improve this with proper testable  *)
let test_unknown_reduce () =
  let open Cast_cic in
  let expected = Lambda { id; dom = Universe 5; body = Unknown (Universe 0) } in
  Alcotest.check Testable.term
    "reduce prod unknown to lambda" expected
    (Reduction.reduce
         (Unknown (Prod { id; dom = Universe 5; body = Universe 0 })))

let test_error_reduce () =
  let open Cast_cic in
  let expected = Lambda { id; dom = Universe 5; body = Err (Universe 0) } in
  Alcotest.check Testable.term
    "reduce prod error to lambda" expected
     (Reduction.reduce
       (Err (Prod { id; dom = Universe 5; body = Universe 0 })))

let tests =
  [
    ("reduce unknown", `Quick, test_unknown_reduce);
    ("reduce error", `Quick, test_error_reduce);
    QCheck_alcotest.to_alcotest strong_normalization;
    QCheck_alcotest.to_alcotest subject_reduction_empty_ctx;
    QCheck_alcotest.to_alcotest progress_empty_ctx;
  ]
