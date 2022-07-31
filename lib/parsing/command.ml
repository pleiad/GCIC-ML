(** This module specifies the AST for commands *)
open Common.Declarations

(** The AST for the commands *)
type 'a t =
  | Eval of 'a
  | Check of 'a
  | Elab of 'a
  | Set of Config.Flag.t
  | Define of 'a const_decl
  | Fix of 'a const_decl
  | Load of string
  | Inductive of 'a ind_decl * 'a ctor_decl list
