open Common.Id
(** This module specifies the structure of the parsed AST *)

(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of (Name.t option * term) list * term
  | Prod of (Name.t option * term) list * term
  | Unknown of int
  | LetIn of (Name.t * term * term * term)

val to_string : term -> string
(** Returns the stringified version of a term *)

val eq_term : term -> term -> bool
(** Equality predicate for terms *)

(** Vernacular commands in GCIC *)
type command = Eval of term | Check of term * term | Elab of term

val string_of_command : command -> string
(** Returns the stringified version of a command *)

val eq_command : command -> command -> bool(** Equality predicate for commands *)
