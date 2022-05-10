open Cast_cic
open Example

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
  let unk0 = unknown 0 in
  Alcotest.check Testable.term "Prod-Unk universe"
    (Lambda { id; dom = Universe 5; body = unk0 })
    (Reduction.reduce
       (Unknown (Prod { id; dom = Universe 5; body = Universe 0 })));
  Alcotest.check Testable.term "Down-Unk universe" unk0
    (Reduction.reduce
       (Cast { source = unk0; target = Universe 0; term = Unknown (unknown 1) }))

let test_error_reduce () =
  let open Ast in
  Alcotest.check Testable.term "Prod-Err universe"
    (Lambda { id; dom = Universe 5; body = Err (Universe 0) })
    (Reduction.reduce (Err (Prod { id; dom = Universe 5; body = Universe 0 })));
  Alcotest.check Testable.term "Down-Err universe" (Err (Universe 0))
    (Reduction.reduce
       (Cast { source = unknown 0; target = Universe 0; term = Err (unknown 1) }))

let test_casts_reduce () =
  let open Ast in
  (let canonical_cast =
     Cast
       {
         source = Prod { id; dom = unknown 0; body = unknown 0 };
         target = unknown 1;
         term = idf;
       }
   in
   Alcotest.check Testable.term "Canonical cast" canonical_cast
     (Reduction.reduce canonical_cast));
  (let unk0 = unknown 0 in
   let inner_cast =
     Cast { source = unk0; target = Universe 0; term = Var id }
   in
   let outer_cast =
     Cast { source = Universe 0; target = unk0; term = inner_cast }
   in
   let term = Lambda { id; dom = unk0; body = outer_cast } in
   Alcotest.check Testable.term "Prod-Germ"
     (Cast
        {
          source = Prod { id; dom = unk0; body = unk0 };
          target = unknown 1;
          term;
        })
     (Reduction.reduce
        (Cast
           {
             source = Prod { id; dom = Universe 0; body = Universe 0 };
             target = unknown 1;
             term = idf;
           })));
  Alcotest.check Testable.term "Dom-Err"
    (Err (unknown 1))
    (Reduction.reduce
       (Cast
          {
            source = Err (Universe 1);
            target =
              Cast
                {
                  source = unknown 1;
                  target = Universe 1;
                  term = Unknown (unknown 1);
                };
            term = idf;
          }));
  Alcotest.check Testable.term "Codom-Err" (Err (Err (Universe 1)))
    (Reduction.reduce
       (Cast { source = Universe 1; target = Err (Universe 1); term = idf }));
  (* This one might be better with a qcheck? *)
  Alcotest.check Testable.term "Univ-Univ" idf
    (Reduction.reduce
       (Cast { source = Universe 1; target = Universe 1; term = idf }));
  (* This one might be better with a qcheck? *)
  Alcotest.check Testable.term "Size-Err Univ"
    (Err (unknown 0))
    (Reduction.reduce
       (Cast { source = Universe 0; target = unknown 0; term = idf }))

(* This is only valid for GCIC variants N and lift *)
let test_omega_reduce =
  let open Ast in
  QCheck.(
    Test.make ~count:100 ~name:"Qcheck Omega fails" small_nat (fun i ->
        assume (i > 0);
        Ast.alpha_equal (Reduction.reduce (omega i)) (Err (unknown (i - 1)))))

let test_inf_prod_elab_reduces () =
  let open Ast in
  Alcotest.check Testable.term "Inf-Prod? reduces" (unknown 0)
    (Reduction.reduce
       (Cast
          {
            source = unknown 1;
            target = Universe 0;
            term = Unknown (unknown 1);
          }))

let tests =
  [
    ("reduce unknown", `Quick, test_unknown_reduce);
    ("reduce error", `Quick, test_error_reduce);
    ("reduce app", `Quick, test_app_reduce);
    ("reduce casts", `Quick, test_casts_reduce);
    QCheck_alcotest.to_alcotest test_omega_reduce;
    ("reduce Inf-Prod?", `Quick, test_inf_prod_elab_reduces);
    QCheck_alcotest.to_alcotest strong_normalization;
    QCheck_alcotest.to_alcotest subject_reduction_empty_ctx;
    QCheck_alcotest.to_alcotest progress_empty_ctx;
  ]
