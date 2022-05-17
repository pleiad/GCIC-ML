open Parsing.Ast

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  Alcotest.testable pprint_term eq_term

let command =
  let pprint_command ppf cmd = Format.pp_print_string ppf (string_of_command cmd) in
  Alcotest.testable pprint_command eq_command

