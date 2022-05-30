(** This module specifies the AST for commands *)

(** The AST for the commands *)
type 'a t =
  | Eval of 'a
  | Check of 'a
  | Elab of 'a
  | Set of Config.Flag.t
  | Define of 'a Kernel.Declarations.const_decl
  | Load of string
