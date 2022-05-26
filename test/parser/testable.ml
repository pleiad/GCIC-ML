open Parsing.Ast

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  Alcotest.testable pprint_term eq_term

let command =
  let open Vernac in
  let def_to_string : term Command.global_definition -> string = function
    | Constant_def def -> Common.Id.Name.to_string def.name
  in
  let eq_definition
      (def1 : term Command.global_definition)
      (def2 : term Command.global_definition)
    =
    match def1, def2 with
    | Constant_def d1, Constant_def d2 ->
      d1.name = d2.name && eq_term d1.ty d2.ty && eq_term d1.term d2.term
  in
  let cmd_to_string : term Command.t -> string = function
    | Eval t -> "eval " ^ to_string t
    | Check t -> "check " ^ to_string t
    | Elab t -> "elab " ^ to_string t
    | SetVariant v -> "set variant " ^ Kernel.Variant.to_string v
    | Definition gdef -> "definition " ^ def_to_string gdef
  in
  let eq_command (cmd1 : term Command.t) (cmd2 : term Command.t) =
    match cmd1, cmd2 with
    | Eval t1, Eval t2 -> eq_term t1 t2
    | Check t1, Check t2 -> eq_term t1 t2
    | Elab t1, Elab t2 -> eq_term t1 t2
    | SetVariant v1, SetVariant v2 -> v1 = v2
    | Definition def1, Definition def2 -> eq_definition def1 def2
    | _ -> false
  in
  let pprint_command ppf cmd = Format.pp_print_string ppf (cmd_to_string cmd) in
  Alcotest.testable pprint_command eq_command
