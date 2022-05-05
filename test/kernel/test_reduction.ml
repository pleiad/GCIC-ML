open Kernel
open Arbitrary

let name_of_int n = string_of_int n |> Cast_cic.Name.of_string

let strong_normalization =
  QCheck.(
    Test.make ~count:1000 ~name:"strong normalization"
      arbitrary_cast_cic_term (fun t ->
        assume (Typing.infer_type Context.empty t |> Result.is_ok);
        Reduction.reduce t |> Cast_cic.is_canonical))

let subject_reduction_empty_ctx =
  let ctx = Context.empty in
  QCheck.(
    Test.make ~count:1000 ~name:"subject reduction"
      arbitrary_cast_cic_term (fun t ->
        let ty = Typing.infer_type ctx t in
        assume (Result.is_ok ty);
        let t' = Reduction.step ctx t in
        assume (Result.is_ok t');
        Typing.check_type ctx (Result.get_ok t') (Result.get_ok ty) |> Result.is_ok))

(* TODO: Improve this with proper testable  *)
let test_unknown_reduce () =
  Alcotest.(check bool)
    "reduce prod unknown to lambda" true
    (match
       Reduction.reduce
         (Unknown
            (Prod { id = name_of_int 1; dom = Universe 5; body = Universe 0 }))
     with
    | Lambda { id = _; dom = Universe 5; body = Unknown (Universe 0) } -> true
    | _ -> false)

let test_error_reduce () =
  Alcotest.(check bool)
    "reduce prod error to lambda" true
    (match
       Reduction.reduce
         (Err (Prod { id = name_of_int 1; dom = Universe 5; body = Universe 0 }))
     with
    | Lambda { id = _; dom = Universe 5; body = Err (Universe 0) } -> true
    | _ -> false)

let tests =
  [
    ("reduce unknown", `Quick, test_unknown_reduce);
    ("reduce error", `Quick, test_error_reduce);
    QCheck_alcotest.to_alcotest strong_normalization;
    QCheck_alcotest.to_alcotest subject_reduction_empty_ctx;
  ]
