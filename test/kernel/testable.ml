open Kernel

let head =
  let open Ast in
  let pprint_head ppf = function
    | HProd -> Fmt.pf ppf "HProd"
    | HUniverse i -> Fmt.pf ppf "HUniverse: %d" i in
  let head_eq a b = a = b in
  Alcotest.testable pprint_head head_eq

let term =
  let open Ast in
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  let term_eq = alpha_equal in
  Alcotest.testable pprint_term term_eq