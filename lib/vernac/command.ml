
(** This module specifies the AST for commands *)

(** The AST for the commands *)
type t =
  | Eval of Kernel.Ast.term
  | Check of Kernel.Ast.term
  | Elab of Kernel.Ast.term
  | SetVariant of Kernel.Variant.t

let to_string : t -> string = function
  | Eval t -> "eval " ^ Kernel.Ast.to_string t
  | Check t -> "check" ^ Kernel.Ast.to_string t
  | Elab t -> "elab " ^ Kernel.Ast.to_string t
  | SetVariant v -> "set variant " ^ Kernel.Variant.to_string v
