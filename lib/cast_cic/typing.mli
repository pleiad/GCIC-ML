(** This module specifies the typing relation *)

val check_context : (Parsing.Ast.Var_name.t, Cast_cic.term) Context.t -> bool  