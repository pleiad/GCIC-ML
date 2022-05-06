let () =
  Alcotest.run "Kernel tests"
    [ ("Context module", Test_context.tests);
      ("Cast_cic module", Test_ast.tests);
      ("Reduction module", Test_reduction.tests);
      ("Typing module", Test_typing.tests);
       ]
