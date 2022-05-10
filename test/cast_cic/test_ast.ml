open! Cast_cic.Ast
open Common

let is_type : term -> bool = function Prod _ | Universe _ -> true | _ -> false

let tests_head () =
  Alcotest.(check (result Testable.head string))
    "head of Prod is Prod" (Ok HProd)
    (head (Prod { id; dom = Universe 1; body = Universe 1 }));
  Alcotest.(check (result Testable.head string))
    "head of Univ is Univ" (Ok (HUniverse 1)) (head (Universe 1))

let tests_head_not_type =
  QCheck.(
    Test.make ~count:20 ~name:"head of not types is error" Arbitrary.term
      (fun t ->
        assume (not (is_type t));
        head t |> Result.is_error))

let tests_is_canonical () =
  Alcotest.(check bool)
    "cast from Prod germ to unknown is canonical" true
    (is_canonical
       (Cast { source = germ 1 HProd; target = unknown 1; term = Universe 57 }));
  Alcotest.(check bool)
    "cast from Prod germ to unknown is not canonical" false
    (is_canonical
       (Cast { source = germ 2 HProd; target = unknown 1; term = Universe 57 }));
  Alcotest.(check bool)
    "cast from Univ germ to unknown is canonical" true
    (is_canonical
       (Cast
          {
            source = germ 1 (HUniverse 0);
            target = unknown 1;
            term = Universe 57;
          }));
  Alcotest.(check bool)
    "cast from Univ germ to unknown is not canonical" true
    (is_canonical
       (Cast
          {
            source = germ 1 (HUniverse 1);
            target = unknown 1;
            term = Universe 57;
          }))

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
    "Apps are equal" true
    (let idx = Name.of_string "x" in
     let idy = Name.of_string "y" in
     let fn id = Lambda { id; dom = unknown 1; body = App (Var id, Var id) } in
     let app1 = App (fn idx, fn idx) in
     let app2 = App (fn idy, fn idy) in
     alpha_equal app1 app2)

let tests_subst () =
  let x = Name.of_string "x" in
  let y = Name.of_string "y" in
  let v = unknown 1 in
  Alcotest.(check Testable.term)
    "Not matching vars are not subst" (Var y) (subst1 x v (Var y));
  Alcotest.(check Testable.term)
    "Lambda subst body included"
    (Lambda { id; dom = v; body = v })
    (subst1 x v (Lambda { id; dom = Var x; body = Var x }));
  Alcotest.(check Testable.term)
    "Lambda subst body not included"
    (Lambda { id = x; dom = v; body = Var x })
    (subst1 x v (Lambda { id = x; dom = Var x; body = Var x }));
  Alcotest.(check Testable.term)
    "Prod subst body included"
    (Prod { id; dom = v; body = v })
    (subst1 x v (Prod { id; dom = Var x; body = Var x }));
  Alcotest.(check Testable.term)
    "Prod subst body not included"
    (Prod { id = x; dom = v; body = Var x })
    (subst1 x v (Prod { id = x; dom = Var x; body = Var x }))

let tests_to_string () =
  let idx = Var (Name.of_string "x") in
  let idy = Var (Name.of_string "y") in
  Alcotest.(check string) "App" "(x y)" (to_string (App (idx, idy)));
  Alcotest.(check string)
    "lambda" "lambda x : ?_y. x"
    (to_string
       (Lambda { id = Name.of_string "x"; dom = Unknown idy; body = idx }));
  Alcotest.(check string)
    "Prod" "Prod x : ▢1. err_▢1"
    (to_string
       (Prod
          { id = Name.of_string "x"; dom = Universe 1; body = Err (Universe 1) }));
  Alcotest.(check string)
    "Cast" "<▢1 <- ▢1> ▢1"
    (to_string
       (Cast { source = Universe 1; target = Universe 1; term = Universe 1 }))

let tests =
  [
    ("head", `Quick, tests_head);
    ("is_canonical", `Quick, tests_is_canonical);
    ("is_neutral", `Quick, tests_is_neutral);
    ("germ", `Quick, tests_germ);
    ("is_germ", `Quick, tests_is_germ);
    ("alpha_equal", `Quick, tests_alpha_equal);
    ("subst", `Quick, tests_subst);
    ("to_string", `Quick, tests_to_string);
    QCheck_alcotest.to_alcotest tests_head_not_type;
    QCheck_alcotest.to_alcotest neutrals_are_canonical;
  ]
