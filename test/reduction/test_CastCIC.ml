open Common.CastCIC
open Utils.Example
open Utils

module ReductionStore : Reduction.Cast_CIC.Store = struct
  let ind_params _ = []
  let const _ = Universe 0
  let ctor_param_args _ = [], []
end

module CastCICReduction = Reduction.Cast_CIC.Make (ReductionStore)

let infer_type = Gcic.CastCIC.CastCICTyping.infer_type
let check_type = Gcic.CastCIC.CastCICTyping.check_type
let reduce t = Gcic.CastCIC.CastCICReduction.reduce t |> Result.map_error Reduction.Cast_CIC.string_of_error

let step = Gcic.CastCIC.CastCICReduction.step
let is_canonical = Gcic.CastCIC.CastCICReduction.is_canonical
let ( <== ) tgt src = tgt, src
let cast (target, source) term = Cast { source; target; term }
let is_true descr pred = Alcotest.(check bool) descr true pred

let tests_is_canonical () =
  let casted_term = Universe 57 in
  let reducible_term = App (idf, Universe 0) in
  is_true
    "lambda with reducible terms is canonical"
    (is_canonical (Lambda { id; dom = reducible_term; body = reducible_term }));
  is_true
    "constructor with reducible terms is canonical"
    (is_canonical
       (Constructor
          { ctor = id; level = 0; params = [ reducible_term ]; args = [ reducible_term ] }));
  is_true
    "product with reducible terms is canonical"
    (is_canonical (Prod { id; dom = reducible_term; body = reducible_term }));
  is_true "universe is canonical" (is_canonical (Universe 0));
  is_true
    "inductive with reducible terms is canonical"
    (is_canonical (Inductive (id, 0, [ reducible_term ])));
  is_true "neutral term is canonical" (is_canonical (Var id));
  is_true "unknown universe is canonical" (is_canonical (unknown 0));
  is_true
    "unknown inductive is canonical"
    (is_canonical (Unknown (Inductive (id, 0, [ reducible_term ]))));
  is_true "unknown of unknown is canonical" (is_canonical (Unknown (unknown 0)));
  is_true "unknown of error is canonical" (is_canonical (Unknown (Err (Universe 0))));
  is_true "error universe is canonical" (is_canonical (Err (Universe 0)));
  is_true
    "error inductive is canonical"
    (is_canonical (Err (Inductive (id, 0, [ reducible_term ]))));
  is_true "error of unknown is canonical" (is_canonical (Err (unknown 0)));
  is_true "error of error is canonical" (is_canonical (Err (Err (Universe 0))));
  is_true
    "cast from Prod germ to unknown at the same level is canonical"
    (is_canonical (cast (unknown 1 <== germ_prod 1) casted_term));
  is_true
    "cast from Prod germ to unknown at different levels is not canonical"
    (is_canonical (cast (unknown 1 <== germ_prod 2) casted_term) |> not);
  is_true
    "cast from Univ germ to unknown is canonical"
    (is_canonical (cast (unknown 1 <== germ_univ 1 0) casted_term));
  is_true
    "cast from Univ germ to unknown at different level is canonical"
    (is_canonical (cast (unknown 1 <== germ_univ 1 1) casted_term))

let strong_normalization =
  QCheck.(
    Test.make ~count:1000 ~name:"strong normalization" Arbitrary.term (fun t ->
        assume (infer_type empty_ctx t |> Result.is_ok);
        reduce t |> Result.get_ok |> is_canonical))

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

let reduces_ok descr exp out = Alcotest.(check (result (Testable.term) string)) descr (Ok exp) out

let test_app_reduce () =
  reduces_ok
    "beta identity"
    (Universe 0)
    (reduce (App (Lambda { id; dom = Universe 1; body = Var id }, Universe 0)))

let test_unknown_reduce () =
  let unk0 = unknown 0 in
  let univ0 = Universe 0 in
  let reducible_term = App (idf, Universe 0) in
  reduces_ok
    "[Prod-Unk] unknown product reduces to lambda"
    (Lambda { id; dom = Universe 5; body = unk0 })
    (reduce (Unknown (Prod { id; dom = Universe 5; body = univ0 })));
  reduces_ok
    "[Match-Unk] unknown propagates through match"
    (Unknown (Universe 0))
    (reduce
       (Match
          { ind = id
          ; discr = Unknown (Inductive (id, 0, []))
          ; z = id
          ; pred = Universe 0
          ; f = id
          ; branches = [ { ctor = id; ids = []; term = reducible_term } ]
          }));
  reduces_ok
    "[Match-Unk] unknown propagates through match and substitutes in pred"
    (Unknown (unknown 1))
    (reduce
       (Match
          { ind = id
          ; discr = Unknown (Inductive (id, 0, []))
          ; z = id
          ; pred = App (Lambda {id; dom=univ0; body=unknown 1}, Var id)
          ; f = id
          ; branches = [ { ctor = id; ids = []; term = reducible_term } ]
          }));
  reduces_ok
    "[Ind-Unk] unknown propagates cast on inductive"
    unk0
    (reduce (cast (univ0 <== unknown 1) (Unknown (unknown 1))));
  reduces_ok
    "[Down-Unk] universe"
    unk0
    (reduce (cast (univ0 <== unknown 1) (Unknown (unknown 1))))

let test_error_reduce () =
  reduces_ok
    "Prod-Err universe"
    (Lambda { id; dom = Universe 5; body = Err (Universe 0) })
    (reduce (Err (Prod { id; dom = Universe 5; body = Universe 0 })));
  reduces_ok
    "Down-Err universe"
    (Err (Universe 0))
    (reduce (Cast { source = unknown 0; target = Universe 0; term = Err (unknown 1) }))

let test_casts_reduce () =
  (let canonical_cast =
     cast (unknown 1 <== Prod { id; dom = unknown 0; body = unknown 0 }) idf
   in
   reduces_ok "Canonical cast" canonical_cast (reduce canonical_cast));
  (let prod_germ = germ_prod 1 in
   reduces_ok
     "Prod-Germ"
     (cast
        (unknown 1 <== prod_germ)
        (cast (prod_germ <== Prod { id; dom = Universe 0; body = Universe 0 }) idf))
     (reduce (cast (unknown 1 <== Prod { id; dom = Universe 0; body = Universe 0 }) idf)));
  reduces_ok
    "Dom-Err"
    (Err (unknown 1))
    (reduce
       (Cast
          { source = Err (Universe 1)
          ; target =
              Cast { source = unknown 2; target = Universe 1; term = Unknown (unknown 2) }
          ; term = idf
          }));
  reduces_ok
    "Codom-Err"
    (Err (Err (Universe 1)))
    (reduce (Cast { source = Universe 1; target = Err (Universe 1); term = idf }));
  (* This one might be better with a qcheck? *)
  reduces_ok
    "Univ-Univ"
    idf
    (reduce (Cast { source = Universe 1; target = Universe 1; term = idf }));
  (* This one might be better with a qcheck? *)
  reduces_ok
    "Size-Err Univ"
    (Err (unknown 0))
    (reduce (Cast { source = Universe 0; target = unknown 0; term = idf }))

(* This is only valid for GCIC variants N and lift *)
let test_omega_reduce =
  QCheck.(
    Test.make ~count:100 ~name:"Qcheck Omega fails" small_nat (fun i ->
        assume (i > 0);
        alpha_equal (reduce (omega i) |> Result.get_ok) (Err (unknown (i - 1)))))

let test_inf_prod_elab_reduces () =
  reduces_ok
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
