let () =
  Alcotest.run "Parser tests"
    [
      ("Term parser", Test_term_parser.tests);
      ("Command parser", Test_command_parser.tests);
    ]
