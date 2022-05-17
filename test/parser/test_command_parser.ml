open Parsing.Ast
open Parsing.Lex_and_parse

let pcommand = Alcotest.(result Testable.command string)

let tests_eval () =
  Alcotest.check
    pcommand
    "Succeds"
    (Ok (Eval (Universe 0)))
    (parse_command "eval Type 0;;");
  Alcotest.(check bool)
    "Fails with :"
    true
    (parse_command "eval Type 0 : ?0;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "eval;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no semi-colons"
    true
    (parse_command "eval ?0 foo" |> Result.is_error)

let tests_check () =
  Alcotest.check
    pcommand
    "check command"
    (Ok (Check (Universe 0, Unknown 0)))
    (parse_command "check Type0 : ?0;;");
  Alcotest.(check bool)
    "Fails without :"
    true
    (parse_command "check Type 0 ?0;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "check;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no semi-colons"
    true
    (parse_command "check ?0 : foo" |> Result.is_error)

let tests_elaborate () =
  Alcotest.check
    pcommand
    "elab command"
    (Ok (Elab (Universe 0)))
    (parse_command "elab Type 0;;");
  Alcotest.(check bool)
    "Fails with :"
    true
    (parse_command "elab Type 0 : ?0;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "elab;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no semi-colons"
    true
    (parse_command "elab ?0 foo" |> Result.is_error)

let tests =
  [ "eval command", `Quick, tests_eval
  ; "check command", `Quick, tests_check
  ; "elab command", `Quick, tests_elaborate
  ]
