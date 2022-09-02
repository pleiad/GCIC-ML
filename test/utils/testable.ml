open Common.CastCIC

(*
let head =
  let open Reduction in
  let pprint_head ppf = function
    | HProd -> Fmt.pf ppf "HProd"
    | HUniverse i -> Fmt.pf ppf "HUniverse: %d" i
    | HInductive ind -> Fmt.pf ppf "HInductive %a" Common.Id.Name.pp ind
  in
  let head_eq a b = a = b in
  Alcotest.testable pprint_head head_eq
  *)

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  let term_eq = alpha_equal in
  Alcotest.testable pprint_term term_eq
