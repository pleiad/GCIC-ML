open Parsing.Ast
open Parsing.Lex_and_parse

let var (id : string) : term = Var (Common.Id.Name.of_string id)

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  let term_eq = eq_term in
  Alcotest.testable pprint_term term_eq

let tests_application () =
  Alcotest.check term
    "nullary application"
    (Universe 0)
    (term_of_string "Type 0");
  Alcotest.check term
    "unary application"
    (App (Unknown 1, Unknown 0))
    (term_of_string "?1 ?0");
  Alcotest.check term
    "n-ary application"
    (App (App (App (var "f", Universe 0), var "x"), var "y"))
    (term_of_string "f (Type 0) x y")

let test_omega () = 
  let delta = Lambda (Common.Id.Name.of_string "x", Unknown 1, App (var "x", var "x")) in
  Alcotest.check term
    "omega at level 0"
    (App (delta, delta))
    (term_of_string "(lambda x : ?1 . x x) (lambda x : ?1 . x x)")

let tests =
  [
    ("application", `Quick, tests_application);
  ]

let examples =
  [
    ("omega", `Quick, test_omega);
  ]

let () = Alcotest.run "Parser tests" [
   ("application", tests);
   ("examples", examples)
    ]