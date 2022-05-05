let () =
  Alcotest.run "Kernel tests"
    [ ("Context module", Test_context.tests);
      ("Cast_cic module", Test_castcic.tests);
      ("Reduction module", Test_reduction.tests);
       ]
