type cmd_result

val string_of_cmd_result : cmd_result -> string

type execute_error

val string_of_error : execute_error -> string
val execute : Kernel.Ast.term Command.t -> (cmd_result, execute_error) result
