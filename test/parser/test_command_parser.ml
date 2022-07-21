open Parsing.Ast
open Parsing.Lex_and_parse

let pcommand = Alcotest.(result Testable.command string)

let tests_eval () =
  Alcotest.check
    pcommand
    "Succeds"
    (Ok (Eval (Universe 0)))
    (parse_command "Eval Type@0.");
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "Eval." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no dot"
    true
    (parse_command "Eval ?0 foo" |> Result.is_error)

let tests_check () =
  Alcotest.check
    pcommand
    "Check command"
    (Ok (Check (Universe 0)))
    (parse_command "Check Type@0.");
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "Check." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no dot"
    true
    (parse_command "Check ?0" |> Result.is_error)

let tests_Elaborate () =
  Alcotest.check
    pcommand
    "Elab command"
    (Ok (Elab (Universe 0)))
    (parse_command "Elab Type@0.");
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "Elab." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no dot"
    true
    (parse_command "Elab ?0 foo" |> Result.is_error)

let tests_Set_Variant () =
  let open Config.Variant in
  Alcotest.check
    pcommand
    "Set Variant command"
    (Ok (Set (Variant G)))
    (parse_command "Set Variant G.");
  Alcotest.check
    pcommand
    "Set Variant command"
    (Ok (Set (Variant N)))
    (parse_command "Set Variant N.");
  Alcotest.check
    pcommand
    "Set Variant command"
    (Ok (Set (Variant S)))
    (parse_command "Set Variant S.");
  Alcotest.(check bool)
    "Fails with not a Variant"
    true
    (parse_command "Set Variant H." |> Result.is_error);
  Alcotest.(check bool)
    "Fails with missing Variant"
    true
    (parse_command "Set Variant." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if missing `Variant` keyword"
    true
    (parse_command "Set G." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if missing `Set` keyword"
    true
    (parse_command "Variant G." |> Result.is_error)

let tests_Set_Fuel () =
  Alcotest.check
    pcommand
    "Set Variant command"
    (Ok (Set (Fuel 0)))
    (parse_command "Set Fuel 0.");
  Alcotest.check
    pcommand
    "Set Variant command"
    (Ok (Set (Fuel 1000)))
    (parse_command "Set Fuel 1000.");
  Alcotest.(check bool)
    "Fails with not an integer"
    true
    (parse_command "Set Fuel H." |> Result.is_error);
  Alcotest.(check bool)
    "Fails with missing integer"
    true
    (parse_command "Set Fuel." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if missing `fail` keyword"
    true
    (parse_command "Set 0." |> Result.is_error)

let tests_Load () =
  Alcotest.check
    pcommand
    "Load command"
    (Ok (Load "file"))
    (parse_command "Load \"file\".");
  Alcotest.(check bool)
    "Fails with not a quoted string"
    true
    (parse_command "Load filename." |> Result.is_error)

let tests =
  [ "Eval command", `Quick, tests_eval
  ; "Check command", `Quick, tests_check
  ; "Elab command", `Quick, tests_Elaborate
  ; "Set Variant command", `Quick, tests_Set_Variant
  ; "Set Fuel command", `Quick, tests_Set_Fuel
  ; "Load command", `Quick, tests_Load
  ]
