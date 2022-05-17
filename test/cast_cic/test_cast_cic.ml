let () =
  Alcotest.run "Kernel tests"
    [
      ("Cast_cic module", Test_ast.tests);
      ("Reduction module", Test_reduction.tests);
      ("Typing module", Test_typing.tests);
      ("Elaboration module", Test_elaboration.tests);
    ]
