(** This module specifies the AST for commands *)

(** The AST for the commands *)
type t =
  | Eval of Kernel.Ast.term
  | Check of Kernel.Ast.term
  | Elab of Kernel.Ast.term
  | SetVariant of Kernel.Variant.t

val to_string : t -> string

