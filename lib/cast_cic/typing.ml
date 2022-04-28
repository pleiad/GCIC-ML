(** This module specifies the typing relation *)

let check_context (ctx : (Parsing.Ast.Var_name.t, Cast_cic.term) Context.t) = 
  Context.is_empty ctx || false 