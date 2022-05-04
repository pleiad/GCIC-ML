open! Kernel.Context

let example1 = add ~key:"x" ~value:1 empty

let tests_is_empty () =
  Alcotest.(check bool) "is_empty empty is true" true (is_empty empty);
  Alcotest.(check bool) "is_empty add is false" false (is_empty example1)

let tests =
    [ ( "is_empty", `Quick, tests_is_empty ) ]