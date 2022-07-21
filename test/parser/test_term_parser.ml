open Parsing.Ast
open Parsing.Lex_and_parse

let name (id : string) = Common.Id.Name.of_string id
let var (id : string) : term = Var (Common.Id.Name.of_string id)
let arg (id : string) = Some (name id)
let funt dom body = Prod ([ None, dom ], body)
let pterm = Alcotest.(result Testable.term string)

let tests_application () =
  Alcotest.check pterm "nullary application" (Ok (Universe 0)) (parse_term "Type0");
  Alcotest.check
    pterm
    "unary application"
    (Ok (App (Unknown 1, Unknown 0)))
    (parse_term "?1 ?0");
  Alcotest.check
    pterm
    "n-ary application"
    (Ok (App (App (App (var "f", Universe 0), var "x"), var "y")))
    (parse_term "f Type0 x y")

let product_notation () =
  Alcotest.check
    pterm
    "non-dependent product notation"
    (Ok (funt (Universe 0) (Universe 1)))
    (parse_term "Type0 -> Type1");
  Alcotest.check
    pterm
    "dependent product forall notation"
    (Ok (Prod ([ arg "x", Universe 0 ], var "x")))
    (parse_term "forall (x : Type0), x");
  Alcotest.check
    pterm
    "n-ary non-dependent product"
    (Ok (funt (Universe 0) (funt (funt (Universe 1) (Universe 2)) (Universe 3))))
    (parse_term "Type0 -> (Type1 -> Type2) -> Type3");
  Alcotest.check
    pterm
    "n-ary non-dependent product"
    (Ok
       (Prod ([ arg "x", Unknown 0; arg "y", funt (Unknown 1) (Universe 2) ], Universe 3)))
    (parse_term "forall (x : ?0) (y : ?1 -> Type2), Type3")

let tests_let () =
  Alcotest.check
    pterm
    "let binding"
    (Ok (LetIn (name "x", Universe 0, Unknown 0, var "x")))
    (parse_term "let x : Type0 := ?0 in x");
  Alcotest.(check bool) "let is reserved" true (parse_term "let" |> Result.is_error);
  Alcotest.(check bool) "in is reserved" true (parse_term "in" |> Result.is_error)

let tests_ascription () =
  Alcotest.check
    pterm
    "ascription"
    (Ok (Ascription (var "x", Unknown 0)))
    (parse_term "x : ?0");
  Alcotest.check
    pterm
    "multiple ascription"
    (Ok
       (Ascription (Ascription (Ascription (Universe 0, Unknown 1), Universe 1), Unknown 2)))
    (parse_term "Type 0 : ?1 : Type1 : ?2");
  Alcotest.(check bool)
    "ascription in lambda arg fails"
    true
    (parse_term "fun (x : ?0 : ?1) => x" |> Result.is_error);
  Alcotest.(check bool)
    "ascription in pi arg fails"
    true
    (parse_term "forall (x : ?0 : ?1), x" |> Result.is_error);
  Alcotest.(check bool)
    "ascription in let binding type fails"
    true
    (parse_term "let x : ?0 : ?1 := Type0 in x" |> Result.is_error);
  Alcotest.(check bool)
    "ascription in let binding body fails"
    true
    (parse_term "let x : ?0 = Type0 : Type1 in x" |> Result.is_error)

let tests_unicode () =
  Alcotest.check pterm "universe" (Ok (Universe 0)) (parse_term "□0");
  Alcotest.check
    pterm
    "lambda"
    (Ok (Lambda ([ arg "x", var "a" ], var "x")))
    (parse_term "λ(x:a)=>x");
  Alcotest.check
    pterm
    "forall"
    (Ok (Prod ([ arg "x", var "a" ], var "x")))
    (parse_term "∀(x:a),x");
  Alcotest.check
    pterm
    "arrow"
    (Ok (Prod ([ None, var "a" ], var "b")))
    (parse_term "a->b")

let tests =
  [ "products", `Quick, product_notation
  ; "application", `Quick, tests_application
  ; "let binding", `Quick, tests_let
  ; "ascriptions", `Quick, tests_ascription
  ; "unicode", `Quick, tests_unicode
  ]
