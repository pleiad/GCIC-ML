(* open Common *)
(** This module specifies the AST for commands *)

(** The AST for the commands *)
type command =
  | Eval of Kernel.Ast.term
  | Check of Kernel.Ast.term * Kernel.Ast.term
  | Elab of Kernel.Ast.term

let string_of_command : command -> string = function
  | Eval t -> "eval " ^ Kernel.Ast.to_string t
  | Check (t, ty) ->
      Format.asprintf "check %s : %s" (Kernel.Ast.to_string t)
        (Kernel.Ast.to_string ty)
  | Elab t -> "elab " ^ Kernel.Ast.to_string t

type cmd_result =
  | Reduction of Cast_cic.Ast.term
  | Check
  | Elaboration of Cast_cic.Ast.term

let string_of_cmd_result : cmd_result -> string = function
  | Reduction t -> Cast_cic.Ast.to_string t
  | Check -> "OK"
  | Elaboration t -> Cast_cic.Ast.to_string t

type execute_error = { error_code : string; message : string; cmd : command }

let string_of_execute_error { error_code; message; _ } =
  "[" ^ error_code ^ "] " ^ message

let reduction_error cmd reason =
  {
    error_code = "reduction_cmd_error";
    message = "reduction failed due to: " ^ reason;
    cmd;
  }

let checking_error cmd reason =
  {
    error_code = "checking_cmd_error";
    message = "checking failed due to: " ^ reason;
    cmd;
  }

let elaboration_error cmd reason =
  {
    error_code = "elaboration_cmd_error";
    message = "elaboration failed due to: " ^ reason;
    cmd;
  }

let mk_elaboration_error (fn : command -> string -> execute_error) cmd
    (e : Cast_cic.Elaboration.elaboration_error) : execute_error =
  let reason = Cast_cic.Elaboration.string_of_error e in
  fn cmd reason

let execute_eval term : (cmd_result, execute_error) result =
  let open Cast_cic in
  match Elaboration.elaborate Context.NameMap.empty term with
  | Ok (elab_term, _) -> Ok (Reduction (Reduction.reduce elab_term))
  | Error e -> Error (mk_elaboration_error reduction_error (Eval term) e)

let execute_check term ty : (cmd_result, execute_error) result =
  let open Cast_cic.Elaboration in
  let open Cast_cic.Typing in
  let open Cast_cic.Context in
  let empty_ctx = NameMap.empty in
  let cmd : command = Check (term, ty) in
  match elaborate empty_ctx term with
  | Ok (elab_term, _) -> (
      match elaborate empty_ctx ty with
      | Ok (expected_ty, _) -> (
          match check_type empty_ctx elab_term expected_ty with
          | Ok _ -> Ok Check
          | Error e -> Error (error_msg e |> checking_error cmd))
      | Error e -> Error (mk_elaboration_error checking_error cmd e))
  | Error e -> Error (mk_elaboration_error checking_error cmd e)

let execute_elab term : (cmd_result, execute_error) result =
  let open Cast_cic.Context in
  match Cast_cic.Elaboration.elaborate NameMap.empty term with
  | Ok (elab_term, _) -> Ok (Elaboration elab_term)
  | Error e -> Error (mk_elaboration_error elaboration_error (Elab term) e)

let execute cmd : (cmd_result, execute_error) result =
  match cmd with
  | Eval t -> execute_eval t
  | Check (t, ty) -> execute_check t ty
  | Elab t -> execute_elab t
