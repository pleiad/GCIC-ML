open! Kernel.Cast_cic
open Arbitrary

(* pprint_foo : Format.formatter -> foo -> unit *)
let pprint_head ppf = function
  | HProd -> Fmt.pf ppf "HProd"
  | HUniverse i -> Fmt.pf ppf "HUniverse: %d" i

let head_eq a b = a = b

(* Pass these two functions in to get a value of type foo testable *)
let head_testable = Alcotest.testable pprint_head head_eq

let tests_head () =
  Alcotest.(check (result head_testable string))
    "head of Var is Error" (Error "invalid term to get head constructor")
    (head (Var (Name.of_string "x")))

let tests_is_neutral () =
  Alcotest.(check bool)
    "cast from error is neutral"
    true
    (is_neutral
       (Cast
          {
            source = Err (Var (Name.of_string "_"));
            target = Unknown (Universe 2);
            term = Universe 57;
          }))

let neutrals_are_canonical =
  QCheck.(
    Test.make ~count:1000 ~name:"neutral terms are canonical"
      arbitrary_cast_cic_term (fun t ->
        assume (is_neutral t);
        is_canonical t))

let tests =
  [
    ("head", `Quick, tests_head);
    ("is_neutral", `Quick, tests_is_neutral);
    QCheck_alcotest.to_alcotest neutrals_are_canonical;
  ]
