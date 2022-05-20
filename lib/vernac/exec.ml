open Common
open Common.Std
open Cast_cic

type cmd_result =
  | Reduction of Ast.term
  | Unit
  | Elaboration of Ast.term
  | Inference of Ast.term

let string_of_cmd_result : cmd_result -> string = function
  | Reduction t -> Ast.to_string t
  | Unit -> "OK"
  | Elaboration t -> Ast.to_string t
  | Inference t -> Ast.to_string t

type execute_error =
  [ Elaboration.elaboration_error
  | Typing.type_error
  | Reduction.reduction_error
  ]

let string_of_error = function
  | #Elaboration.elaboration_error as e ->
    "[elaboration_error] " ^ Elaboration.string_of_error e
  | #Typing.type_error as e -> "[type_error] " ^ Typing.string_of_error e
  | #Reduction.reduction_error as e -> "[reduction_error] " ^ Reduction.string_of_error e

let global_definitions : (Ast.term option * Ast.term) Context.t ref = ref Context.empty

let execute_eval term : (cmd_result, execute_error) result =
  let open Elaboration in
  let open Reduction in
  let ctx = Context.map snd !global_definitions in
  let ctx_v = Context.map fst !global_definitions in
  let* elab_term, _ = elaborate ctx term in
  let* v = reduce (Ast.subst ctx_v elab_term) in
  Ok (Reduction v)

let execute_check term : (cmd_result, execute_error) result =
  let open Elaboration in
  let open Typing in
  let open Reduction in
  let ctx = Context.map snd !global_definitions in
  let ctx_v = Context.map fst !global_definitions in
  let* elab_term, _ = elaborate ctx term in
  let* ty = infer_type ctx elab_term in
  let* v = reduce (Ast.subst ctx_v ty) in
  Ok (Inference v)

let execute_elab term : (cmd_result, execute_error) result =
  let open Elaboration in
  let ctx = Context.map snd !global_definitions in
  let* elab_term, _ = elaborate ctx term in
  Ok (Elaboration elab_term)

let execute_set_variant var : (cmd_result, execute_error) result =
  Kernel.Variant.set_variant var;
  Ok Unit

let execute_definition gdef : (cmd_result, execute_error) result =
  let open Elaboration in
  let open Reduction in
  let open Command in
  let ctx_v = Context.map fst !global_definitions in
  let ctx = Context.map snd !global_definitions in
  match gdef with
  | Constant_def { name; ty; term } ->
    let* elab_ty, _ = elab_univ ctx ty in
    let* norm_ty = reduce (Ast.subst ctx_v elab_ty) in
    let* elab_term = check_elab ctx term norm_ty in
    let* norm_term = reduce (Ast.subst ctx_v elab_term) in
    global_definitions := Context.add name (Some norm_term, norm_ty) !global_definitions;
    Ok Unit

let execute cmd : (cmd_result, execute_error) result =
  let open Command in
  match cmd with
  | Eval t -> execute_eval t
  | Check t -> execute_check t
  | Elab t -> execute_elab t
  | SetVariant v -> execute_set_variant v
  | Definition gdef -> execute_definition gdef
