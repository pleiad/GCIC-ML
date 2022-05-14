open Parsing.Ast
open Parsing.Lex_and_parse

let name (id : string) = Common.Id.Name.of_string id
let var (id : string) : term = Var (Common.Id.Name.of_string id)
let arg (id : string) = Some (name id)

let funt dom body = Prod ([(None, dom)], body)

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  Alcotest.testable pprint_term eq_term

let pterm = Alcotest.(result term string)

let tests_application () =
  Alcotest.check pterm
    "nullary application"
    (Ok (Universe 0))
    (term_of_string "Type0");
  Alcotest.check pterm
    "unary application"
    (Ok (App (Unknown 1, Unknown 0)))
    (term_of_string "?1 ?0");
  Alcotest.check pterm
    "n-ary application"
    (Ok (App (App (App (var "f", Universe 0), var "x"), var "y")))
    (term_of_string "f Type0 x y")

let test_omega () = 
  let delta = Lambda ([(arg "x", Unknown 1)], App (var "x", var "x")) in
  Alcotest.check pterm
    "omega at level 0"
    (Ok (App (delta, delta)))
    (term_of_string "(fun (x : ?1) . x x) (fun (x : ?1) . x x)")

let product_notation () =
  Alcotest.check pterm
    "non-dependent product notation"
    (Ok (funt (Universe 0) (Universe 1)))
    (term_of_string "Type0 -> Type1");
  Alcotest.check pterm
    "dependent product forall notation"
    (Ok (Prod ([(arg "x", Universe 0)], var "x")))
    (term_of_string "forall (x : Type0), x");
  Alcotest.check pterm
    "n-ary non-dependent product"
    (Ok (funt (Universe 0) (funt (funt (Universe 1) (Universe 2)) (Universe 3))))
    (term_of_string "Type0 -> (Type1 -> Type2) -> Type3");
  Alcotest.check pterm
    "n-ary non-dependent product"
    (Ok (Prod ([(arg "x", Unknown 0); (arg "y", funt (Unknown 1) (Universe 2))], Universe 3)))
    (term_of_string "forall (x : ?0) (y : ?1 -> Type2), Type3")

let tests_unicode () =
  Alcotest.check pterm
    "universe"
    (Ok (Universe 0))
    (term_of_string "□0");
  Alcotest.check pterm
    "lambda"
    (Ok (Lambda ([(arg "x", var "a")], var "x")))
    (term_of_string "λ(x:a).x");
  Alcotest.check pterm
    "forall"
    (Ok (Prod ([(arg "x", var "a")], var "x")))
    (term_of_string "∀(x:a),x");
  Alcotest.check pterm
    "arrow"
    (Ok (Prod ([(None, var "a")], var "b")))
    (term_of_string "a->b")

let tests =
  [
    ("products", `Quick, product_notation );
    ("application", `Quick, tests_application);
    ("unicode", `Quick, tests_unicode);
  ]

let () = Alcotest.run "Parser tests" [ ("Parser module", tests); ]
