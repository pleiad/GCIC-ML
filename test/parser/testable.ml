open Parsing.Ast

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  Alcotest.testable pprint_term eq_term

let command =
  let cmd_to_string : term Vernac.Command.t -> string = function
    | Eval t -> "eval " ^ to_string t
    | Check t -> "check" ^ to_string t
    | Elab t -> "elab " ^ to_string t
    | SetVariant v -> "set variant " ^ Kernel.Variant.to_string v
  in
  let eq_command (cmd1 : term Vernac.Command.t) (cmd2 : term Vernac.Command.t) =
    match cmd1, cmd2 with
    | Eval t1, Eval t2 -> eq_term t1 t2
    | Check t1, Check t2 -> eq_term t1 t2
    | Elab t1, Elab t2 -> eq_term t1 t2
    | SetVariant v1, SetVariant v2 -> v1 = v2
    | _ -> false
  in
  let pprint_command ppf cmd = Format.pp_print_string ppf (cmd_to_string cmd) in
  Alcotest.testable pprint_command eq_command
