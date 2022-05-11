(** This module specifies the structure of CastCIC *)
open Common

val new_identifier : unit -> Id.Name.t

(** Terms in CastCIC *)
type term =
  | Var of Id.Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of term
  | Err of term
  | Cast of { source : term; target : term; term : term }

and fun_info = { id : Id.Name.t; dom : term; body : term }

val to_string : term -> string

type context = (Id.Name.t, term) Context.t
(** Context *)

(** GCIC variants: Gradual, Normalizing and Shift *)
type gcic_variant = G | N | S

val gcic_variant : gcic_variant
(** Parameter specifying the GCIC variant *)

val product_universe_level : int -> int -> int
(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)

val cast_universe_level : int -> int
(** Computes the level of the universe for a cast between (? -> ?) and ? *)

(** Head constructors *)
type head = HProd | HUniverse of int

val head : term -> (head, string) result
(** Returns the head constructor of a type *)

val germ : int -> head -> term
(** Returns the least precise type for the given head constructor, 
    at the provided level *)

val is_germ : int -> term -> bool
(** Checks if a term corresponds to a germ at the provided universe level *)

val is_germ_for_gte_level : int -> term -> bool
(** Checks if a term corresponds to a germ for a level >= to the provided universe level *)

val is_neutral : term -> bool
(** Checks if a term is in neutral form *)

val is_canonical : term -> bool
(** Checks if a term is in canonical form *)

val subst1 : Id.Name.t -> term -> term -> term
(** Performs substitution inside a vterm *)

val alpha_equal : term -> term -> bool
(** Checks if two terms are identifiable up to alpha-renaming *)

val alpha_consistent : term -> term -> bool 
(** Checks if two terms are alpha consistent *)