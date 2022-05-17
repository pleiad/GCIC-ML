open! Common.Context

let example1 = add ~key:"x" ~value:1 empty

let tests_is_empty () =
  Alcotest.(check bool) "is_empty empty is true" true (is_empty empty);
  Alcotest.(check bool) "is_empty add is false" false (is_empty example1)

let test_of_to_list =
  let open QCheck in
  Test.make ~count:1000 ~name:"to_list . of_list = id"
    (list @@ pair string int)
    (fun l -> to_list (of_list l) = l)

let test_lookup () =
  Alcotest.(check (option int))
    "lookup singleton" (Some 1)
    (lookup ~key:"x" ~ctx:example1);
  Alcotest.(check (option int)) "lookup empty" None (lookup ~key:"x" ~ctx:empty);
  Alcotest.(check (option int))
    "lookup missing" None
    (lookup ~key:"y" ~ctx:example1)

let test_remove () =
  Alcotest.(check bool)
    "remove empty ctx" true
    (is_empty (remove ~key:"x" ~ctx:empty));
  Alcotest.(check bool)
    "remove at head" true
    (is_empty (remove ~key:"x" ~ctx:example1));
  Alcotest.(check (option int))
    "remove in between" None
    (lookup ~key:"x"
       ~ctx:(remove ~key:"x" ~ctx:(add ~key:"y" ~value:2 example1)))

let test_to_string () =
  let string_of_key k = k in
  let string_of_value = string_of_int in
  let to_str = to_string string_of_key string_of_value in
  Alcotest.(check string) "to_string empty ctx" "[]" (to_str empty);
  Alcotest.(check string)
    "to_string non-empty ctx" "(y, 2) ; (x, 1) ; []"
    (to_str (add ~key:"y" ~value:2 example1))

let tests =
  [
    ("is_empty", `Quick, tests_is_empty);
    QCheck_alcotest.to_alcotest test_of_to_list;
    ("lookup key", `Quick, test_lookup);
    ("remove key", `Quick, test_remove);
    ("to_string", `Quick, test_to_string);
  ]
