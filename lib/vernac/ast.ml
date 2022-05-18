open Common.Std

(** This module specifies the AST for commands *)

(** The AST for the commands *)
type command =
  | Eval of Kernel.Ast.term
  | Check of Kernel.Ast.term * Kernel.Ast.term
  | Elab of Kernel.Ast.term
  | Set of Config.t

let string_of_command : command -> string = function
  | Eval t -> "eval " ^ Kernel.Ast.to_string t
  | Check (t, ty) ->
    Format.asprintf "check %s : %s" (Kernel.Ast.to_string t) (Kernel.Ast.to_string ty)
  | Elab t -> "elab " ^ Kernel.Ast.to_string t
  | Set cfg -> "set " ^ Config.to_string cfg

type cmd_result =
  | Reduction of Cast_cic.Ast.term
  | Unit
  | Elaboration of Cast_cic.Ast.term

let string_of_cmd_result : cmd_result -> string = function
  | Reduction t -> Cast_cic.Ast.to_string t
  | Unit -> "OK"
  | Elaboration t -> Cast_cic.Ast.to_string t

type execute_error =
  [ Cast_cic.Elaboration.elaboration_error
  | Cast_cic.Typing.type_error
  | Cast_cic.Reduction.reduction_error
  ]

let string_of_error = function
  | #Cast_cic.Elaboration.elaboration_error as e ->
    "[elaboration_error] " ^ Cast_cic.Elaboration.string_of_error e
  | #Cast_cic.Typing.type_error as e ->
    "[type_error] " ^ Cast_cic.Typing.string_of_error e
  | #Cast_cic.Reduction.reduction_error as e ->
    "[reduction_error] " ^ Cast_cic.Reduction.string_of_error e

let execute_eval term : (cmd_result, execute_error) result =
  let open Cast_cic.Elaboration in
  let open Cast_cic.Reduction in
  let open Cast_cic.Context in
  let* elab_term, _ = elaborate NameMap.empty term in
  let* v = reduce elab_term in
  Ok (Reduction v)

let execute_check term ty : (cmd_result, execute_error) result =
  let open Cast_cic.Elaboration in
  let open Cast_cic.Typing in
  let open Cast_cic.Context in
  let empty_ctx = NameMap.empty in
  let* elab_term, _ = elaborate empty_ctx term in
  let* expected_ty, _ = elaborate empty_ctx ty in
  let* () = check_type empty_ctx elab_term expected_ty in
  Ok Unit

let execute_elab term : (cmd_result, execute_error) result =
  let open Cast_cic.Elaboration in
  let open Cast_cic.Context in
  let* elab_term, _ = elaborate NameMap.empty term in
  Ok (Elaboration elab_term)

let execute_set_config : Config.t -> (cmd_result, execute_error) result = function
  | Variant v ->
    Kernel.Variant.set_variant v;
    Ok Unit
  | Fuel i ->
    Cast_cic.Reduction.set_fuel i;
    Ok Unit

let execute cmd : (cmd_result, execute_error) result =
  match cmd with
  | Eval t -> execute_eval t
  | Check (t, ty) -> execute_check t ty
  | Elab t -> execute_elab t
  | Set cfg -> execute_set_config cfg
