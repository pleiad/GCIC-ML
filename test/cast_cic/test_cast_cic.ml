let () =
  Alcotest.run
    "Kernel tests"
    [ "Cast_cic module", Test_ast.tests
    ; "Typing module", Test_typing.tests
    ; "Elaboration module", Test_elaboration.tests
    ]
