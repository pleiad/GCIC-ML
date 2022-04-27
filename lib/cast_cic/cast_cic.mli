(** This module specifies the structure of CastCIC *)

(** An abstract type for identifiers*)
module type ID = sig
    type t
  
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
  end
  
module Var_name : ID 

(** Terms in CastCIC *)
type term =
| Var of Var_name.t
| Universe of int 
| App of term * term 
| Lambda of Var_name.t * term * term 
| Prod of Var_name.t * term * term 
| Unknown of term 
| Err of term 
| Cast of term * term

(** GCIC variants: Gradual, Normalizing and Shift *)
type gcic_variant = G | N | S

(** Parameter specifying the GCIC variant *)
val gcic_variant : gcic_variant

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
val product_universe_level : int -> int -> int 

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
val cast_universe_level : int -> int

(** Head constructors *)
type head =
| Prod 
| Universe of int
(* | Inductive *)

(** Returns the head constructor of a type *)
val head : term -> (head, string) result 

(** Returns the least precise type for the given head constructor, 
    at the provided level *)
val germ : int -> head -> term 
