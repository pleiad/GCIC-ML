(** This module specifies the AST for commands *)

(** The AST for the commands *)
type command =
  | Eval of Kernel.Ast.term
  | Check of Kernel.Ast.term * Kernel.Ast.term
  | Elab of Kernel.Ast.term
  | Set of Config.t

val string_of_command : command -> string

type cmd_result

val string_of_cmd_result : cmd_result -> string

type execute_error

val string_of_error : execute_error -> string
val execute : command -> (cmd_result, execute_error) result
