open Kernel

let varname x = Ast.Name.of_string x

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
  let tests = [  ]
