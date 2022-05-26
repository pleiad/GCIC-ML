(** This module specifies the AST for commands *)

(** The AST for the commands *)
type 'a t =
  | Eval of 'a
  | Check of 'a
  | Elab of 'a
  | SetVariant of Kernel.Variant.t
  | Definition of 'a global_definition
  | Import of string 

and 'a global_definition =
  | Constant_def of
      { name : Common.Id.Name.t
      ; ty : 'a
      ; term : 'a
      }
