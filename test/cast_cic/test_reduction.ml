open Common.CastCIC
open Utils.Example
open Utils

let infer_type = Gcic.CastCIC.CastCICTyping.infer_type
let check_type = Gcic.CastCIC.CastCICTyping.check_type
let reduce t = Gcic.CastCIC.CastCICReduction.reduce t |> Result.get_ok
let step = Gcic.CastCIC.CastCICReduction.step
let is_canonical = Gcic.CastCIC.CastCICReduction.is_canonical

let tests_is_canonical () =
  Alcotest.(check bool)
    "cast from Prod germ to unknown is canonical"
    true
    (is_canonical (Cast { source = germ_prod 1; target = unknown 1; term = Universe 57 }));
  Alcotest.(check bool)
    "cast from Prod germ to unknown is not canonical"
    false
    (is_canonical (Cast { source = germ_prod 2; target = unknown 1; term = Universe 57 }));
  Alcotest.(check bool)
    "cast from Univ germ to unknown is canonical"
    true
    (is_canonical
       (Cast { source = germ_univ 1 0; target = unknown 1; term = Universe 57 }));
  Alcotest.(check bool)
    "cast from Univ germ to unknown is not canonical"
    true
    (is_canonical
       (Cast { source = germ_univ 1 1; target = unknown 1; term = Universe 57 }))

let strong_normalization =
  QCheck.(
    Test.make ~count:1000 ~name:"strong normalization" Arbitrary.term (fun t ->
        assume (infer_type empty_ctx t |> Result.is_ok);
        reduce t |> is_canonical))

let subject_reduction_empty_ctx =
  let ctx = empty_ctx in
  QCheck.(
    Test.make ~count:1000 ~name:"subject reduction in empty ctx" Arbitrary.term (fun t ->
        let ty = infer_type ctx t in
        assume (Result.is_ok ty);
        let t' = step t in
        assume (Result.is_ok t');
        check_type ctx (Result.get_ok t') (Result.get_ok ty) |> Result.is_ok))

let progress_empty_ctx =
  let ctx = empty_ctx in
  QCheck.(
    Test.make ~count:1000 ~name:"progress in empty ctx" Arbitrary.term (fun t ->
        assume (infer_type ctx t |> Result.is_ok);
        is_canonical t || step t |> Result.is_ok))

let test_app_reduce () =
  Alcotest.check
    Testable.term
    "beta identity"
    (Universe 0)
    (reduce (App (Lambda { id; dom = Universe 1; body = Var id }, Universe 0)))

let test_unknown_reduce () =
  let unk0 = unknown 0 in
  Alcotest.check
    Testable.term
    "Prod-Unk universe"
    (Lambda { id; dom = Universe 5; body = unk0 })
    (reduce (Unknown (Prod { id; dom = Universe 5; body = Universe 0 })));
  Alcotest.check
    Testable.term
    "Down-Unk universe"
    unk0
    (reduce
       (Cast { source = unknown 1; target = Universe 0; term = Unknown (unknown 1) }))

let test_error_reduce () =
  Alcotest.check
    Testable.term
    "Prod-Err universe"
    (Lambda { id; dom = Universe 5; body = Err (Universe 0) })
    (reduce (Err (Prod { id; dom = Universe 5; body = Universe 0 })));
  Alcotest.check
    Testable.term
    "Down-Err universe"
    (Err (Universe 0))
    (reduce (Cast { source = unknown 0; target = Universe 0; term = Err (unknown 1) }))

let test_casts_reduce () =
  (let canonical_cast =
     Cast
       { source = Prod { id; dom = unknown 0; body = unknown 0 }
       ; target = unknown 1
       ; term = idf
       }
   in
   Alcotest.check Testable.term "Canonical cast" canonical_cast (reduce canonical_cast));
  (let prod_germ = germ_prod 1 in
   Alcotest.check
     Testable.term
     "Prod-Germ"
     (Cast
        { source = prod_germ
        ; target = unknown 1
        ; term =
            Cast
              { source = Prod { id; dom = Universe 0; body = Universe 0 }
              ; target = prod_germ
              ; term = idf
              }
        })
     (reduce
        (Cast
           { source = Prod { id; dom = Universe 0; body = Universe 0 }
           ; target = unknown 1
           ; term = idf
           })));
  Alcotest.check
    Testable.term
    "Dom-Err"
    (Err (unknown 1))
    (reduce
       (Cast
          { source = Err (Universe 1)
          ; target =
              Cast { source = unknown 2; target = Universe 1; term = Unknown (unknown 2) }
          ; term = idf
          }));
  Alcotest.check
    Testable.term
    "Codom-Err"
    (Err (Err (Universe 1)))
    (reduce (Cast { source = Universe 1; target = Err (Universe 1); term = idf }));
  (* This one might be better with a qcheck? *)
  Alcotest.check
    Testable.term
    "Univ-Univ"
    idf
    (reduce (Cast { source = Universe 1; target = Universe 1; term = idf }));
  (* This one might be better with a qcheck? *)
  Alcotest.check
    Testable.term
    "Size-Err Univ"
    (Err (unknown 0))
    (reduce (Cast { source = Universe 0; target = unknown 0; term = idf }))

(* This is only valid for GCIC variants N and lift *)
let test_omega_reduce =
  QCheck.(
    Test.make ~count:100 ~name:"Qcheck Omega fails" small_nat (fun i ->
        assume (i > 0);
        alpha_equal (reduce (omega i)) (Err (unknown (i - 1)))))

let test_inf_prod_elab_reduces () =
  Alcotest.check
    Testable.term
    "Inf-Prod? reduces"
    (unknown 0)
    (reduce
       (Cast { source = unknown 1; target = Universe 0; term = Unknown (unknown 1) }))

let tests =
  [ "reduce unknown", `Quick, test_unknown_reduce
  ; "reduce error", `Quick, test_error_reduce
  ; "reduce app", `Quick, test_app_reduce
  ; "reduce casts", `Quick, test_casts_reduce
  ; QCheck_alcotest.to_alcotest test_omega_reduce
  ; "reduce Inf-Prod?", `Quick, test_inf_prod_elab_reduces
  ; "is_canonical", `Quick, tests_is_canonical
  ; QCheck_alcotest.to_alcotest strong_normalization
  ; QCheck_alcotest.to_alcotest subject_reduction_empty_ctx
  ; QCheck_alcotest.to_alcotest progress_empty_ctx
  ]
