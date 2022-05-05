open! Kernel.Context

let example1 = add ~key:"x" ~value:1 empty

let tests_is_empty () =
  Alcotest.(check bool) "is_empty empty is true" true (is_empty empty);
  Alcotest.(check bool) "is_empty add is false" false (is_empty example1)

let test_of_to_list =
  let open QCheck in
  Test.make ~count:1000 ~name:"to_list . of_list = id"
    (list @@ pair string int)
    (fun l -> to_list (of_list l) = l)

let tests =
    [ ( "is_empty", `Quick, tests_is_empty );
      QCheck_alcotest.to_alcotest test_of_to_list ]