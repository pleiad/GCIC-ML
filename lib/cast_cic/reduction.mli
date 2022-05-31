(** This module specifies the operational semantics *)

open Ast

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of term
  | `Err_free_const
  ]

val string_of_error : reduction_error -> string

(** Head constructors *)
type head =
  | HProd
  | HUniverse of int
  | HInductive of Common.Id.Name.t

(** Returns the head constructor of a type *)
val head : term -> (head, string) result

(** Returns the least precise type for the given head constructor, 
    at the provided level *)
val germ : int -> head -> term

(** Checks if a term corresponds to a germ at the provided universe level *)
val is_germ : int -> term -> bool

(** Checks if a term corresponds to a germ for a level >= to the provided universe level *)
val is_germ_for_gte_level : int -> term -> bool

(** Checks if a term is in neutral form *)
val is_neutral : term -> bool

(** Checks if a term is in canonical form *)
val is_canonical : term -> bool

(** Reduces a term *)
val reduce : term -> (term, [> reduction_error ]) result

(** One step reduction *)
val step : term -> (term, [> reduction_error ]) result
