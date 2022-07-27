(** This module specifies the structure of CastCIC *)
open Id

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
      { (* We keep track of the inductive to be used in the reduction of a match 
      expression. The match is turned into a lambda with the inductive as domain. *)
        ind : Name.t
      ; discr : term
      ; z : Name.t
      ; pred : term
      ; f : Name.t
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

(** Performs substitution inside a term *)
val subst : term Name.Map.t -> term -> term

(** Performs substitution inside a term *)
val subst1 : Name.t -> term -> term -> term

(** Checks if two terms are identifiable up to alpha-renaming *)
val alpha_equal : term -> term -> bool

(** Telescopic substitution of values for free variables in a list of terms. 
    For instance, the values of an inductive or a constructor and the declared
    parameter and argument types, such as in `cons Bool ...`:
    - cons types: (A : Type) (hd : A) (tl : list A)
    - values : Bool ... 
    - result : [Type, Bool, list Bool]
    Raises an error if the values and the list of terms have different sizes.
*)
val subst_tele : ?acc:term list -> term list -> (Name.t * term) list -> term list

(** Telescopic substitution of a single value for the first free variable in a
    list of terms. Similar to `subst_tele` but just the first identifier in the 
    rest of the list. 
*)
val subst1_tele : term -> (Name.t * term) list -> (Name.t * term) list
