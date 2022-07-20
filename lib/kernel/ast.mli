(** This module specifies the structure of GCIC *)
open Common.Id

(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of int
  (* Inductives *)
  | Inductive of Name.t * int * term list
  | Constructor of Name.t * term list
  | Match of
      { ind : Name.t
      ; discr : term
      ; z : Name.t
      ; pred : term
      ; f : Name.t
      ; branches : branch list
      }
  (* Extras *)
  | Ascription of term * term
  | UnknownT of int
  | Const of Name.t

and fun_info =
  { id : Name.t
  ; dom : term
  ; body : term
  }

and branch =
  { ctor : Name.t
  ; ids : Name.t list
  ; term : term
  }

(** Pretty printers *)
val pp_term : Format.formatter -> term -> unit

(** Returns the prettified version of a term *)
val to_string : term -> string

(** Prints the prettified version of a term *)
val print : term -> unit

(** Equality predicate between terms *)
val eq : term -> term -> bool

val get_sort_lvl : term -> int
