(** This module specifies the AST for commands *)

(** The AST for the commands *)
type 'a t =
  | Eval of 'a
  | Check of 'a
  | Elab of 'a
  | SetVariant of Kernel.Variant.t


