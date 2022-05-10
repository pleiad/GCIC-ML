open Cast_cic
open Common

let varname x = Ast.Name.of_string x

let test_omega_typecheck =
  QCheck.(
    Test.make ~count:10 ~name:"omega i > 0 typechecks" small_nat (fun i ->
        assume (i > 0);
        Ast.alpha_equal (Typing.infer_type empty_ctx (omega i) |> Result.get_ok) (unknown (i - 1))))

let test_omega_typecheck_0 () =
  Alcotest.(check bool) "omega 0 doesn't typechecks" 
    true
    (Typing.infer_type empty_ctx (omega 0) |> Result.is_error)

(* let tests_is_empty () =
  Alcotest.(check (result (bool, string)))
    "is_empty empty is true" true
    (Typing.infer_type Context.empty
       (Unknown (Prod { id = varname "_"; dom = Universe 5; body = Universe 0 }))
    |> Result.map Cast_cic.is_canonical
    |> Result.is_ok);
  Alcotest.(check bool) "is_empty add is false" false false *)

  (* let tests_is_canonical () =
    Alcotest.(check bool)
    "is_canonical unknown" *)

    (* ("is_empty", `Quick, tests_is_empty) *)
  let tests = [
    ("omega 0 doesn't typecheck", `Quick, test_omega_typecheck_0);
    QCheck_alcotest.to_alcotest test_omega_typecheck;
    ]
