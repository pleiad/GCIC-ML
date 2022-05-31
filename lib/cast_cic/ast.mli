(** This module specifies the structure of CastCIC *)
open Common.Id

val new_identifier : unit -> Name.t

(** Terms in CastCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of term
  | Err of term
  | Cast of
      { source : term
      ; target : term
      ; term : term
      }
  | Const of Name.t
  (* Inductives *)
  | Inductive of Name.t * int * term list
  | Constructor of
      { ctor : Name.t
      ; level : int
      ; params : term list
      ; args : term list
      }
  | Match of
      { ind : Name.t
      ; discr : term
      ; z : Name.t
      ; pred : term
      ; branches : branch list
      }

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

(** Checks if a term is in neutral form *)
val is_neutral : term -> bool

(** Checks if a term is in canonical form *)
val is_canonical : term -> bool

(** Performs substitution inside a term *)
val subst : term option Name.Map.t -> term -> term

(** Performs substitution inside a term *)
val subst1 : Name.t -> term -> term -> term

(** Checks if two terms are identifiable up to alpha-renaming *)
val alpha_equal : term -> term -> bool

(** Checks if two terms are alpha consistent *)
val alpha_consistent : term -> term -> bool
