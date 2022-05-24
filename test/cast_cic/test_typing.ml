open Cast_cic
open Example
open Common.Id

let varname x = Name.of_string x

let test_omega_typecheck =
  QCheck.(
    Test.make ~count:10 ~name:"omega i > 0 typechecks" small_nat (fun i ->
        assume (i > 0);
        Ast.alpha_equal
          (Typing.infer_type empty_ctx (omega i) |> Result.get_ok)
          (unknown (i - 1))))

let test_omega_typecheck_0 () =
  Alcotest.(check bool)
    "omega 0 doesn't typechecks"
    true
    (Typing.infer_type empty_ctx (omega 0) |> Result.is_error)

let tests =
  [ "omega 0 doesn't typecheck", `Quick, test_omega_typecheck_0
  ; QCheck_alcotest.to_alcotest test_omega_typecheck
  ]
