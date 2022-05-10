open! Cast_cic.Ast
open Common

let tests_head () =
  Alcotest.(check (result Testable.head string))
    "head of Var is Error" (Error "invalid term to get head constructor")
    (head (Var (Name.of_string "x")));
  Alcotest.(check (result Testable.head string))
    "head of Prod is Prod" (Ok HProd)
    (head (Prod { id; dom = Universe 1; body = Universe 1 }));
  Alcotest.(check (result Testable.head string))
    "head of Univ is Univ" (Ok (HUniverse 1)) (head (Universe 1))

let tests_is_neutral () =
  Alcotest.(check bool)
    "cast from error is neutral" true
    (is_neutral
       (Cast
          {
            source = Err (Var (Name.of_string "_"));
            target = Unknown (Universe 2);
            term = Universe 57;
          }))

let neutrals_are_canonical =
  QCheck.(
    Test.make ~count:1000 ~name:"neutral terms are canonical" Arbitrary.term
      (fun t ->
        assume (is_neutral t);
        is_canonical t))

let tests_germ () =
  Alcotest.(check Testable.term)
    "germ of Univ is Univ at lower level" (Universe 0) (germ 1 (HUniverse 0));
  Alcotest.(check Testable.term)
    "germ of Univ is Error at gte level" (Err (Universe 0))
    (germ 0 (HUniverse 0))

let tests_is_germ () =
  Alcotest.(check bool)
    "Univ at lower level is germ" true (is_germ 1 (Universe 0));
  Alcotest.(check bool)
    "Univ is not germ at gte level" false (is_germ 0 (Universe 0))

let tests_alpha_equal () = 
  Alcotest.(check bool)
    "Apps are equal" true (
      let idx = Name.of_string "x" in
      let idy = Name.of_string "y" in
      let fn id = Lambda { id; dom= unknown 1; body= App (Var id, Var id)} in 
      let app1 = App (fn idx, fn idx) in 
      let app2 = App (fn idy, fn idy) in 
      alpha_equal app1 app2
    )

let tests =
  [
    ("head", `Quick, tests_head);
    ("is_neutral", `Quick, tests_is_neutral);
    ("germ", `Quick, tests_germ);
    ("is_germ", `Quick, tests_is_germ);
    ("alpha_equal", `Quick, tests_alpha_equal);
    QCheck_alcotest.to_alcotest neutrals_are_canonical;
  ]
