(** This module specifies the different GCIC variants and 
    the interface surrounding them *)

(** GCIC variants: Gradual, Normalizing and Shift *)
type t =
  | G (** Gradual *)
  | N (** Normalizing *)
  | S (** Shift *)

(** Returns the stringified version of a variant *)
val to_string : t -> string

(** Sets the current active variant *)
val set_variant : t -> unit

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
val product_universe_level : int -> int -> int

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
val cast_universe_level : int -> int
