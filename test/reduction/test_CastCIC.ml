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

let reduce t =
  Gcic.CastCIC.CastCICReduction.reduce t
  |> Result.map_error Reduction.Cast_CIC.string_of_error

let step = Gcic.CastCIC.CastCICReduction.step
let is_canonical = Gcic.CastCIC.CastCICReduction.is_canonical
let is_neutral = Gcic.CastCIC.CastCICReduction.is_neutral
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

let tests_is_neutral () =
  let casted_term = Universe 57 in
  is_true "var is neutral" (is_neutral (Var id));
  is_true "app is neutral with neutral lhs" (is_neutral (App (Var id, Universe 0)));
  is_true
    "match is neutral with neutral discriminee"
    (is_neutral
       (Match
          { ind = id; discr = Var id; z = id; pred = Universe 0; f = id; branches = [] }));
  is_true "unknown is neutral with neutral type" (is_neutral (Unknown (Var id)));
  is_true "error is neutral with neutral type" (is_neutral (Err (Var id)));
  is_true
    "cast is neutral with neutral source type"
    (is_neutral (cast (Universe 0 <== Var id) casted_term));
  is_true
    "cast from unknown is neutral with neutral term"
    (is_neutral (cast (Universe 0 <== unknown 0) (Var id)));
  is_true
    "cast from universe is neutral with neutral target"
    (is_neutral (cast (Var id <== Universe 0) casted_term));
  is_true
    "cast from product is neutral with neutral target"
    (is_neutral
       (cast (Var id <== Prod { id; dom = Universe 0; body = Universe 0 }) casted_term));
  is_true
    "cast between products is neutral with neutral term"
    (is_neutral
       (cast
          (Prod { id; dom = Universe 1; body = Universe 1 }
          <== Prod { id; dom = Universe 0; body = Universe 0 })
          (Var id)));
  is_true
    "cast from inductive is neutral with neutral target"
    (is_neutral (cast (Var id <== Inductive (id, 0, [])) casted_term));
  is_true
    "cast between inductives is neutral with neutral term"
    (is_neutral (cast (Inductive (id, 1, []) <== Inductive (id, 0, [])) (Var id)))

let canonicity =
  QCheck.(
    Test.make ~count:1000 ~name:"canonicity" Arbitrary.term (fun t ->
        assume (infer_type empty_ctx t |> Result.is_ok);
        reduce t |> Result.fold ~ok:is_canonical ~error:(fun _ -> false)))

let subject_reduction_empty_ctx =
  QCheck.(
    Test.make
      ~count:1000
      ~name:"subject reduction in empty context"
      Arbitrary.term
      (fun t ->
        let ty = infer_type empty_ctx t in
        assume (Result.is_ok ty);
        let t' = step t in
        assume (Result.is_ok t');
        check_type empty_ctx (Result.get_ok t') (Result.get_ok ty) |> Result.is_ok))

let progress_empty_ctx =
  QCheck.(
    Test.make ~count:1000 ~name:"progress in empty ctx" Arbitrary.term (fun t ->
        assume (infer_type empty_ctx t |> Result.is_ok);
        is_canonical t || step t |> Result.is_ok))

(* Auxiliary function to make tests more readable *)
let reduces_ok descr exp out =
  Alcotest.(check (result Testable.term string)) descr (Ok exp) out

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
          ; pred = App (Lambda { id; dom = univ0; body = unknown 1 }, Var id)
          ; f = id
          ; branches = [ { ctor = id; ids = []; term = reducible_term } ]
          }));
  reduces_ok
    "[Ind-Unk] unknown propagates cast on inductive"
    (Unknown (Inductive (id, 0, [])))
    (reduce
       (cast
          (Inductive (id, 0, []) <== Inductive (id, 0, [ Universe 0 ]))
          (Unknown (Inductive (id, 0, [ Universe 0 ])))));
  reduces_ok
    "[Down-Unk] universe"
    unk0
    (reduce (cast (univ0 <== unknown 1) (Unknown (unknown 1))));
  reduces_ok
    "[Down-Unk] product"
    (Lambda { id; dom = Universe 0; body = unknown 0 })
    (reduce
       (cast
          (Prod { id; dom = Universe 0; body = Universe 0 } <== unknown 1)
          (Unknown (unknown 1))))

let test_error_reduce () =
  let reducible_term = App (idf, Universe 0) in
  reduces_ok
    "[Prod-Err] error product reduces to lambda"
    (Lambda { id; dom = Universe 5; body = Err (Universe 0) })
    (reduce (Err (Prod { id; dom = Universe 5; body = Universe 0 })));
  reduces_ok
    "[Match-Err] error propagates through match"
    (Err (Universe 0))
    (reduce
       (Match
          { ind = id
          ; discr = Err (Inductive (id, 0, []))
          ; z = id
          ; pred = Universe 0
          ; f = id
          ; branches = [ { ctor = id; ids = []; term = reducible_term } ]
          }));
  reduces_ok
    "[Match-Err] error propagates through match and substitutes in pred"
    (Err (unknown 1))
    (reduce
       (Match
          { ind = id
          ; discr = Err (Inductive (id, 0, []))
          ; z = id
          ; pred = App (Lambda { id; dom = Universe 0; body = unknown 1 }, Var id)
          ; f = id
          ; branches = [ { ctor = id; ids = []; term = reducible_term } ]
          }));
  reduces_ok
    "[Err-Unk] error propagates cast on inductive"
    (Err (Inductive (id, 0, [])))
    (reduce
       (cast
          (Inductive (id, 0, []) <== Inductive (id, 0, [ Universe 0 ]))
          (Err (Inductive (id, 0, [ Universe 0 ])))));
  reduces_ok
    "[Down-Err] universe"
    (Err (Universe 0))
    (reduce (cast (Universe 0 <== unknown 1) (Err (unknown 1))));
  reduces_ok
    "[Down-Err] product"
    (Lambda { id; dom = Universe 0; body = Err (Universe 0) })
    (reduce
       (cast
          (Prod { id; dom = Universe 0; body = Universe 0 } <== unknown 1)
          (Err (unknown 1))))

let test_casts_reduce () =
  (let canonical_cast =
     cast (unknown 1 <== Prod { id; dom = unknown 0; body = unknown 0 }) idf
   in
   reduces_ok "Canonical cast" canonical_cast (reduce canonical_cast));
   reduces_ok
   "[Prod-Prod] cast lambda from product to product is a lambda"
   (* As per the paper, the inner cast is to the domain of the function *)
    (Lambda
       { id = idy
       ; dom = Universe 1
       ; body =
           cast (Universe 1 <== Universe 0) (cast (Universe 0 <== Universe 1) (Var idy))
       })
    (reduce
       (cast
          (Prod { id = idy; dom = Universe 1; body = Universe 1 }
          <== Prod { id = idx; dom = Universe 0; body = Universe 0 })
          (Lambda { id = idx; dom = Universe 3; body = Var idx })));
  (* This one might be better with a qcheck? *)
  reduces_ok
    "[Univ-Univ] Any type casted from universe to same universe eliminates cast"
    idf
    (reduce (cast (Universe 1 <== Universe 1) idf));
  (* reduces_ok
    "[Ind-Ind] casting empty constructor between inductives is the same"
    (Constructor { ctor = id_of "ctor"; level = 0; params = []; args = [] })
    (reduce
       (cast
          (Inductive (id, 0, [Universe 0]) <== Inductive (id, 0, []))
          (Constructor { ctor = id_of "ctor"; level = 0; params = []; args = [] }))); *)
  reduces_ok
    "[Head-Err] Casting from product to universe is an error of universe"
    (Err (Universe 0))
    (reduce (cast (Universe 0 <== Prod { id; dom = Universe 0; body = Universe 0 }) idf));
  reduces_ok
    "[Head-Err] Casting from universe to product is an error of product"
    (Lambda { id; dom = Universe 0; body = Err (Universe 1) })
    (reduce (cast (Prod { id; dom = Universe 0; body = Universe 1 } <== Universe 1) idf));
  reduces_ok
    "[Head-Err] Casting from universe to inductive is an error of inductive"
    (Err (Inductive (id, 0, [ Universe 0 ])))
    (reduce (cast (Inductive (id, 0, [ Universe 0 ]) <== Universe 1) idf));
  reduces_ok
    "[Dom-Err] Casting from error to any type is an error of the type"
    (Err (Universe 0))
    (reduce (cast (Universe 0 <== Err (Universe 1)) idf));
  reduces_ok
    "[Codom-Err] Casting from any type to error is an error of error"
    (Err (Err (Universe 1)))
    (reduce (cast (Err (Universe 1) <== Universe 1) idf));
  reduces_ok
    "[Prod-Germ] Casting from product to unknown adds intermediate cast to germ"
    (cast
       (unknown 1 <== germ_prod 1)
       (cast (germ_prod 1 <== Prod { id; dom = Universe 0; body = Universe 0 }) idf))
    (reduce (cast (unknown 1 <== Prod { id; dom = Universe 0; body = Universe 0 }) idf));
  (* reduces_ok
    "[Ind-Germ] Casting from inductive to unknown adds intermediate cast to germ"
    (cast
       (unknown 1 <== Inductive (id, 0, [ unknown 0 ]))
       (cast (Inductive (id, 0, [ unknown 0 ]) <== Inductive (id, 0, [ Universe 0 ])) idf))
    (reduce (cast (unknown 1 <== Inductive (id, 0, [ Universe 0 ])) idf)); *)
  (* This one might be better with a qcheck? *)
  reduces_ok
    "[Up-Down] Casting from universe germ through unknown removes intermediate cast"
    idf
    (reduce (cast (Universe 0 <== unknown 1) (cast (unknown 1 <== Universe 0) idf)));
  reduces_ok
    "[Up-Down] Casting from product germ through unknown removes intermediate cast"
    (Lambda
       { id = idy
       ; dom = Universe 1
       ; body =
           cast (Universe 1 <== unknown 0) (cast (unknown 0 <== Universe 1) (Var idy))
       })
    (reduce
       (cast
          (Prod { id = idy; dom = Universe 1; body = Universe 1 } <== unknown 1)
          (cast
             (unknown 1 <== germ_prod 1)
             (Lambda { id = idx; dom = Universe 0; body = Var idx }))));
  reduces_ok
    "Size-Err Univ"
    (Err (unknown 0))
    (reduce (cast (unknown 0 <== Universe 0) idf))

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
  ; "is_neutral", `Quick, tests_is_neutral
  ; QCheck_alcotest.to_alcotest canonicity
  ; QCheck_alcotest.to_alcotest subject_reduction_empty_ctx
  ; QCheck_alcotest.to_alcotest progress_empty_ctx
  ]
