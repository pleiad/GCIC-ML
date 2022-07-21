(** This module specifies the execution of commands *)

(** The type of succesfully executing commands *)
type cmd_result =
  | Reduction of Cast_cic.Ast.term
  | Unit
  | Elaboration of Cast_cic.Ast.term
  | Inference of Cast_cic.Ast.term
  | Definition of Common.Id.Name.t * Kernel.Ast.term

(** Returns the stringified version of the command's result *)
val string_of_cmd_result : cmd_result -> string

(** The type of errors occurring during the execution of commands *)
type execute_error

(** Returns the stringified version of an error *)
val string_of_error : execute_error -> string

(** Executes a command *)
val execute
  :  (string -> Kernel.Ast.term Command.t list)
  -> Kernel.Ast.term Command.t
  -> (cmd_result, execute_error) result
