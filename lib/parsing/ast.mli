(** This module specifies the structure of the parsed AST *)
open Common.Id

(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of (Name.t option * term) list * term
  | Prod of (Name.t option * term) list * term
  | Unknown of int
  | Match of
      { ind : Name.t
      ; discr : term
      ; z : Name.t
      ; pred : term
      ; branches : branch list
      }
  (* Extras *)
  | LetIn of (Name.t * term * term * term)
  | Ascription of term * term
  | UnknownT of int

and branch =
  { ctor : Name.t
  ; ids : Name.t list
  ; body : term
  }

(** Returns the stringified version of a term *)
val to_string : term -> string

(** Equality predicate for terms *)
val eq_term : term -> term -> bool
