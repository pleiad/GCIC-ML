open Parsing
open Parsing.Ast

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  Alcotest.testable pprint_term eq_term

let command =
  let open Common.Declarations in
  let def_to_string ({ name; _ } : term const_decl) = Common.Id.Name.to_string name in
  let eq_definition (d1 : term const_decl) (d2 : term const_decl) =
    d1.name = d2.name && eq_term d1.ty d2.ty && eq_term d1.term d2.term
  in
  let cmd_to_string : term Command.t -> string = function
    | Eval t -> "eval " ^ to_string t
    | Check t -> "check " ^ to_string t
    | Elab t -> "elab " ^ to_string t
    | Set flag -> "set " ^ Config.Flag.to_string flag
    | Define gdef -> "definition " ^ def_to_string gdef
    | Load filename -> Format.asprintf "import \"%s\"" filename
    | Inductive (ind, _ctors) ->
      Format.asprintf "inductive %s" (Common.Id.Name.to_string ind.name)
  in
  let eq_command (cmd1 : term Command.t) (cmd2 : term Command.t) =
    match cmd1, cmd2 with
    | Eval t1, Eval t2 -> eq_term t1 t2
    | Check t1, Check t2 -> eq_term t1 t2
    | Elab t1, Elab t2 -> eq_term t1 t2
    | Set flag1, Set flag2 -> flag1 = flag2
    | Define def1, Define def2 -> eq_definition def1 def2
    | Load fn1, Load fn2 -> fn1 = fn2
    | _ -> false
  in
  let pprint_command ppf cmd = Format.pp_print_string ppf (cmd_to_string cmd) in
  Alcotest.testable pprint_command eq_command
