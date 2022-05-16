(** This module specifies the AST for commands *)

(** The AST for the commands *)
type command =
| Eval of Kernel.Ast.term
| Check of Kernel.Ast.term * Kernel.Ast.term
| Elaborate of Kernel.Ast.term 

val string_of_command : command -> string

type cmd_result 

val string_of_cmd_result : cmd_result -> string 

type execute_error = {
    error_code: string; 
    message: string;
    cmd: command
}

val execute : command -> (cmd_result, execute_error) result