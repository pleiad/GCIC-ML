(** This module specifies the AST for commands *)
open Common.Id

(** The AST for the commands *)
type 'a t =
  | Eval of 'a
  | Check of 'a
  | Elab of 'a
  | Set of Config.Flag.t
  | Define of 'a global_definition
  | Load of string

and 'a global_definition =
  | Constant_def of
      { name : Name.t
      ; ty : 'a
      ; term : 'a
      }
  | Inductive_def of
      { name : Name.t
      ; params : (Name.t * 'a) list
      ; sort : 'a
      ; ctors : Name.t list
      }
  | Constructor_def of
      { name : Name.t
      ; ind : Name.t
      ; params : (Name.t * 'a) list
      ; args : (Name.t * 'a) list
      }
