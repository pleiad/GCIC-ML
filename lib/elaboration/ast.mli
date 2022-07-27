(** This module specifies the structure of CastCIC *)
open Common.Id

val new_identifier : unit -> Name.t

(** Terms in CastCIC *)
type elaborated_term =
  | Var of Name.t
  | Universe of int
  | App of elaborated_term * elaborated_term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of elaborated_term
  | Err of elaborated_term
  | Cast of
      { source : elaborated_term
      ; target : elaborated_term
      ; term : elaborated_term
      }
  | Const of Name.t
  (* Inductives *)
  | Inductive of Name.t * int * elaborated_term list
  | Constructor of
      { ctor : Name.t
      ; level : int
      ; params : elaborated_term list
      ; args : elaborated_term list
      }
  | Match of
      { (* We keep track of the inductive to be used in the reduction of a match 
      expression. The match is turned into a lambda with the inductive as domain. *)
        ind : Name.t
      ; discr : elaborated_term
      ; z : Name.t
      ; pred : elaborated_term
      ; f : Name.t
      ; branches : branch list
      }

and fun_info =
  { id : Name.t
  ; dom : elaborated_term
  ; body : elaborated_term
  }

and branch =
  { ctor : Name.t
  ; ids : Name.t list
  ; term : elaborated_term
  }

(** Pretty printers *)
val pp_term : Format.formatter -> elaborated_term -> unit

(** Returns the prettified version of a term *)
val to_string : elaborated_term -> string

(** Prints the prettified version of a term *)
val print : elaborated_term -> unit

(** Performs substitution inside a term *)
val subst : elaborated_term Name.Map.t -> elaborated_term -> elaborated_term

(** Performs substitution inside a term *)
val subst1 : Name.t -> elaborated_term -> elaborated_term -> elaborated_term

(** Checks if two terms are identifiable up to alpha-renaming *)
val alpha_equal : elaborated_term -> elaborated_term -> bool

(** Telescopic substitution of values for free variables in a list of terms. 
    For instance, the values of an inductive or a constructor and the declared
    parameter and argument types, such as in `cons Bool ...`:
    - cons types: (A : Type) (hd : A) (tl : list A)
    - values : Bool ... 
    - result : [Type, Bool, list Bool]
    Raises an error if the values and the list of terms have different sizes.
*)
val subst_tele
  :  ?acc:elaborated_term list
  -> elaborated_term list
  -> (Name.t * elaborated_term) list
  -> elaborated_term list

(** Telescopic substitution of a single value for the first free variable in a
    list of terms. Similar to `subst_tele` but just the first identifier in the 
    rest of the list. 
*)
val subst1_tele
  :  elaborated_term
  -> (Name.t * elaborated_term) list
  -> (Name.t * elaborated_term) list
