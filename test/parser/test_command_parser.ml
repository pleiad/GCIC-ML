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
    (Ok (Check (Universe 0)))
    (parse_command "check Type0;;");
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "check;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no semi-colons"
    true
    (parse_command "check ?0" |> Result.is_error)

let tests_elaborate () =
  Alcotest.check
    pcommand
    "elab command"
    (Ok (Elab (Universe 0)))
    (parse_command "elab Type 0;;");
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "elab;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no semi-colons"
    true
    (parse_command "elab ?0 foo" |> Result.is_error)

let tests_set_variant () =
  let open Kernel.Variant in
  Alcotest.check
    pcommand
    "set variant command"
    (Ok (SetVariant G))
    (parse_command "set variant G;;");
  Alcotest.check
    pcommand
    "set variant command"
    (Ok (SetVariant N))
    (parse_command "set variant N;;");
  Alcotest.check
    pcommand
    "set variant command"
    (Ok (SetVariant S))
    (parse_command "set variant S;;");
  Alcotest.(check bool)
    "Fails with not a variant"
    true
    (parse_command "set variant H;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails with missing variant"
    true
    (parse_command "set variant;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if missing `variant` keyword"
    true
    (parse_command "set G;;" |> Result.is_error);
  Alcotest.(check bool)
    "Fails if missing `set` keyword"
    true
    (parse_command "variant G;;" |> Result.is_error)

let tests_load () =
  Alcotest.check
    pcommand
    "load command"
    (Ok (Load "file"))
    (parse_command "load \"file\";;");
  Alcotest.(check bool)
    "Fails with not a quoted string"
    true
    (parse_command "load filename;;" |> Result.is_error)

let tests =
  [ "eval command", `Quick, tests_eval
  ; "check command", `Quick, tests_check
  ; "elab command", `Quick, tests_elaborate
  ; "set variant command", `Quick, tests_set_variant
  ; "load command", `Quick, tests_load
  ]
