open! Kernel.Cast_cic

let%expect_test "Test head Var" =
  print_string (Result.get_error (head (Var (Name.of_string "x")))) ;
  [%expect {|invalid term to get head constructor|}]