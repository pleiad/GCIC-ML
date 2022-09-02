let () =
  Alcotest.run
    "Reduction tests"
    [ "Reduction module", Test_CastCIC.tests
    ]
