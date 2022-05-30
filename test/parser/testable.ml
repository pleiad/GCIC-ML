open Parsing.Ast

let term =
  let pprint_term ppf t = Format.pp_print_string ppf (to_string t) in
  Alcotest.testable pprint_term eq_term

let command =
  let open Vernac in
  let def_to_string : term Command.global_definition -> string = function
    | Constant_def def -> Common.Id.Name.to_string def.name
    | Inductive_def def -> Common.Id.Name.to_string def.name
    | Constructor_def def -> Common.Id.Name.to_string def.name
  in
  let eq_arg (id1, t1) (id2, t2) = id1 = id2 && eq_term t1 t2 in
  let eq_definition
      (def1 : term Command.global_definition)
      (def2 : term Command.global_definition)
    =
    match def1, def2 with
    | Constant_def d1, Constant_def d2 ->
      d1.name = d2.name && eq_term d1.ty d2.ty && eq_term d1.term d2.term
    | Inductive_def d1, Inductive_def d2 ->
      d1.name = d2.name
      && List.for_all2 eq_arg d1.params d2.params
      && eq_term d1.sort d2.sort
      && List.equal ( = ) d1.ctors d2.ctors
    | Constructor_def d1, Constructor_def d2 ->
      d1.name = d2.name
      && d1.ind = d2.ind
      && List.for_all2 eq_arg d1.params d2.params
      && List.for_all2 eq_arg d1.args d2.args
    | _, _ -> false
  in
  let cmd_to_string : term Command.t -> string = function
    | Eval t -> "eval " ^ to_string t
    | Check t -> "check " ^ to_string t
    | Elab t -> "elab " ^ to_string t
    | Set flag -> "set " ^ Config.Flag.to_string flag
    | Define gdef -> "definition " ^ def_to_string gdef
    | Load filename -> Format.asprintf "import \"%s\"" filename
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
