open Cast_cic

let name_of_int n = string_of_int n |> Ast.Name.of_string
let id = Ast.Name.of_string "x"

let idf =
  let open Ast in
  Lambda { id; dom = Universe 0; body = Var id }

let unknown i = Ast.Unknown (Ast.Universe i)

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

(* This one actually gets stuck, which is fine - but it should be considered ok, right? *)
let t =
  let open Ast in
  Cast
    {
      source = Prod { id; dom = Universe 1; body = Universe 1 };
      target = Unknown (Universe 1);
      term = Lambda { id; dom = Universe 1; body = Var id };
    }

let test_casts_reduce () =
  let open Ast in
  (let canonical_cast =
     Cast
       {
         source =
           Prod { id; dom = Unknown (Universe 0); body = Unknown (Universe 0) };
         target = Unknown (Universe 1);
         term = idf;
       }
   in
   Alcotest.check Testable.term "Canonical cast" canonical_cast
     (Reduction.reduce canonical_cast));
  let unk0 = unknown 0 in
  let inner_cast = Cast { source = unk0; target = Universe 0; term = Var id } in
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
          }))

(* From the GCIC paper, this is the elaboration of delta (from which omega is built) *)
let delta' i =
  let open Ast in
  let dom =
    Cast
      {
        source = unknown (i + 1);
        target = Universe i;
        term = Unknown (unknown (i + 1));
      }
  in
  Lambda
    {
      id;
      dom;
      body =
        App
          ( Cast { source = dom; target = germ i HProd; term = Var id },
            Cast
              {
                source = dom;
                target = unknown (cast_universe_level i);
                term = Var id;
              } );
    }

let omega i =
  let open Ast in
  let d' = delta' i in
  let dom =
    Cast
      {
        source = Unknown (Universe (i + 1));
        target = Universe i;
        term = Unknown (Unknown (Universe (i + 1)));
      }
  in
  App
    ( d',
      Cast
        {
          source =
            Prod { id; dom; body = unknown (cast_universe_level i) };
          target = dom;
          term = d';
        } )

(* This is only valid for GCIC variants N and lift *)
let test_omega_reduce () =
  let open Ast in
  Alcotest.check Testable.term "Omega fails" (Err (Unknown (Universe 0)))
    (Reduction.reduce (omega 1))

let test_inf_prod_elab_reduces () =
  let open Ast in
  Alcotest.check Testable.term "Inf-Prod? reduces" (Unknown (Universe 0))
    (Reduction.reduce
       (Cast
          {
            source = Unknown (Universe 1);
            target = Universe 0;
            term = Unknown (Unknown (Universe 1));
          }))

let tests =
  [
    ("reduce unknown", `Quick, test_unknown_reduce);
    ("reduce error", `Quick, test_error_reduce);
    ("reduce app", `Quick, test_app_reduce);
    ("reduce casts", `Quick, test_casts_reduce);
    ("reduce omega", `Quick, test_omega_reduce);
    ("reduce Inf-Prod?", `Quick, test_inf_prod_elab_reduces);
    QCheck_alcotest.to_alcotest strong_normalization;
    QCheck_alcotest.to_alcotest subject_reduction_empty_ctx;
    QCheck_alcotest.to_alcotest progress_empty_ctx;
  ]
