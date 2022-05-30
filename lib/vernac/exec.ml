open Common.Std
open Common.Id
open Cast_cic

type cmd_result =
  | Reduction of Ast.term
  | Unit
  | Elaboration of Ast.term
  | Inference of Ast.term
  | Definition of Name.t * Kernel.Ast.term

let string_of_cmd_result : cmd_result -> string = function
  | Reduction t -> Ast.to_string t
  | Unit -> "OK"
  | Elaboration t -> Ast.to_string t
  | Inference t -> Ast.to_string t
  | Definition (name, ty) ->
    Name.to_string name ^ " : " ^ Kernel.Ast.to_string ty ^ " defined."

type execute_error =
  [ Elaboration.elaboration_error
  | Typing.type_error
  | Reduction.reduction_error
  | `LoadError of execute_error
  | `FileNotFound of string
  ]

let rec string_of_error = function
  | #Elaboration.elaboration_error as e ->
    "[elaboration_error] " ^ Elaboration.string_of_error e
  | #Typing.type_error as e -> "[type_error] " ^ Typing.string_of_error e
  | #Reduction.reduction_error as e -> "[reduction_error] " ^ Reduction.string_of_error e
  | `LoadError e -> "[load_error]\n" ^ string_of_error e
  | `FileNotFound filename -> "[file_not_found_error] " ^ filename

let execute_eval term : (cmd_result, execute_error) result =
  let open Elaboration in
  let open Reduction in
  let* elab_term, _ = elaborate reduce Name.Map.empty term in
  let* v = reduce elab_term in
  Ok (Reduction v)

let execute_check term : (cmd_result, execute_error) result =
  let open Elaboration in
  let open Typing in
  let open Reduction in
  let empty_ctx = Name.Map.empty in
  let* elab_term, _ = elaborate reduce empty_ctx term in
  let* ty = infer_type empty_ctx elab_term in
  let* v = reduce ty in
  (* By default it normalizes the output. TODO: Add flag *)
  Ok (Inference v)

let execute_elab term : (cmd_result, execute_error) result =
  let open Elaboration in
  let* elab_term, _ = elaborate Reduction.reduce Name.Map.empty term in
  Ok (Elaboration elab_term)

let execute_set_flag (flag : Config.Flag.t) : (cmd_result, execute_error) result =
  Config.set_flag flag;
  Ok Unit

let execute_definition gdef : (cmd_result, execute_error) result =
  let open Elaboration in
  let open Typing in
  let open Reduction in
  let open Kernel.Declarations in
  let empty_ctx = Name.Map.empty in
  match gdef with
  | { name; ty; term } ->
    let* elab_ty, _ = elab_univ reduce empty_ctx ty in
    let* elab_term = check_elab reduce empty_ctx term elab_ty in
    let* _ = check_type empty_ctx elab_term elab_ty in
    Kernel.Declarations.Const.add name { name; ty; term };
    Ok (Definition (name, ty))

exception LoadFail of execute_error

let rec execute file_parser cmd : (cmd_result, execute_error) result =
  let open Command in
  match cmd with
  | Eval t -> execute_eval t
  | Check t -> execute_check t
  | Elab t -> execute_elab t
  | Set f -> execute_set_flag f
  | Define gdef -> execute_definition gdef
  | Load filename -> execute_load file_parser filename

and execute_load file_parser filename =
  try
    let src = Stdio.In_channel.read_all filename in
    let cmds = file_parser src in
    List.iter
      (fun cmd ->
        match execute file_parser cmd with
        | Ok res -> string_of_cmd_result res |> print_endline
        | Error e -> raise (LoadFail e))
      cmds;
    Ok Unit
  with
  | LoadFail e -> Error (`LoadError e)
  | Sys_error _ -> Error (`FileNotFound filename)
