open! Kernel.Context

let%expect_test "Test is_empty" =
  print_string (is_empty empty |> string_of_bool) ;
  [%expect {|true|}]