open Parsing.Ast
open Parsing.Lex_and_parse

let name (id : string) = Common.Id.Name.of_string id
let var (id : string) : term = Var (Common.Id.Name.of_string id)

let funt dom body = Prod (name "_", dom, body)

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
    (term_of_string "(fun x : ?1 . x x) (fun x : ?1 . x x)")

let product_notation () =
  Alcotest.check term
    "non-dependent product notation"
    (funt (Universe 0) (Universe 1))
    (term_of_string "Type0 -> Type1");
  Alcotest.check term
    "dependent product notation"
    (Prod (name "x", Universe 0, var "x"))
    (term_of_string "(x : Type0) -> x");
  Alcotest.check term
    "n-ary non-dependent product"
    (funt (Universe 0) (funt (funt (Universe 1) (Universe 2)) (Universe 3)))
    (term_of_string "Type0 -> (Type1 -> Type2) -> Type3");
  Alcotest.check term
    "n-ary non-dependent product"
    (Prod (name "x", Unknown 0, (Prod (name "y", funt (Unknown 1) (Universe 2), (Universe 3)))))
    (term_of_string "(x : ?0) -> (y : ?1 -> Type2) -> Type3")

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
   ("products", [ "notation", `Quick, product_notation ]);
   ("examples", examples)
    ]