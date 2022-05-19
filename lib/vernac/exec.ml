open Common.Std
open Common.Id

type cmd_result =
  | Reduction of Cast_cic.Ast.term
  | Unit
  | Elaboration of Cast_cic.Ast.term
  | Inference of Cast_cic.Ast.term

let string_of_cmd_result : cmd_result -> string = function
  | Reduction t -> Cast_cic.Ast.to_string t
  | Unit -> "OK"
  | Elaboration t -> Cast_cic.Ast.to_string t
  | Inference t -> Cast_cic.Ast.to_string t

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

let execute_check term : (cmd_result, execute_error) result =
  let open Cast_cic.Elaboration in
  let open Cast_cic.Typing in
  let open Cast_cic.Reduction in
  let open Cast_cic.Context in
  let empty_ctx = NameMap.empty in
  let* elab_term, _ = elaborate empty_ctx term in
  let* ty = infer_type empty_ctx elab_term in
  let* v = reduce ty in
  Ok (Inference v)

let execute_elab term : (cmd_result, execute_error) result =
  let open Cast_cic.Elaboration in
  let open Cast_cic.Context in
  let* elab_term, _ = elaborate NameMap.empty term in
  Ok (Elaboration elab_term)

let execute_set_variant var : (cmd_result, execute_error) result =
  Kernel.Variant.set_variant var;
  Ok Unit

let execute cmd : (cmd_result, execute_error) result =
  let open Command in
  match cmd with
  | Eval t -> execute_eval t
  | Check t -> execute_check t
  | Elab t -> execute_elab t
  | SetVariant v -> execute_set_variant v
  | Definition (_id, _args, _body) -> Ok Unit
