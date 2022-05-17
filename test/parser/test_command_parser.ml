open Parsing.Ast
open Parsing.Lex_and_parse

let pcommand = Alcotest.(result Testable.command string)

let tests_eval () =
  Alcotest.check pcommand
    "Succeds"
    (Ok (Eval (Universe 0)))
    (parse_command "Eval Type 0.");
  Alcotest.(check bool)
    "Fails with As"
    true
    (parse_command "Eval Type 0 As ?0." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "Eval." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no dot"
    true
    (parse_command "Eval ?0 foo" |> Result.is_error)

let tests_check () =
  Alcotest.check pcommand
    "Check command"
    (Ok (Check (Universe 0, Unknown 0)))
    (parse_command "Check Type0 As ?0.");
  Alcotest.(check bool)
    "Fails without As"
    true
    (parse_command "Check Type 0 ?0." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "Check." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no dot"
    true
    (parse_command "Check ?0 As foo" |> Result.is_error)

let tests_elaborate () =
  Alcotest.check pcommand
    "Elaborate command"
    (Ok (Elaborate (Universe 0)))
    (parse_command "Elaborate Type 0.");
  Alcotest.(check bool)
    "Fails with As"
    true
    (parse_command "Elaborate Type 0 As ?0." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no expression"
    true
    (parse_command "Elaborate." |> Result.is_error);
  Alcotest.(check bool)
    "Fails if no dot"
    true
    (parse_command "Elaborate ?0 foo" |> Result.is_error)

let tests =
  [
    ("Eval command", `Quick, tests_eval );
    ("Check command", `Quick, tests_check );
    ("Elaborate command", `Quick, tests_elaborate );
  ]
