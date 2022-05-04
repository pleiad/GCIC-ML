open! Kernel.Cast_cic

(* pprint_foo : Format.formatter -> foo -> unit *)
let pprint_head ppf =  function
| HProd -> Fmt.pf ppf "HProd"
| HUniverse i -> Fmt.pf ppf "HUniverse: %d" i

let head_eq a b = (a=b)

(* Pass these two functions in to get a value of type foo testable *)
let head_testable = Alcotest.testable pprint_head head_eq

let tests_head () =
  Alcotest.(check (result head_testable string))
    "head of Var is Error" (Error "invalid term to get head constructor")
    (head (Var (Name.of_string "x")))

let tests =
    [ ( "head", `Quick, tests_head ) ]