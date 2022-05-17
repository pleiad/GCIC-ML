(** This module specifies the different GCIC variants and 
    the interface surrounding them *)

(** GCIC variants: Gradual, Normalizing and Shift *)
type gcic_variant =
  | G
  | N
  | S

(** Sets the current active variant *)
val set_variant : gcic_variant -> unit

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
val product_universe_level : int -> int -> int

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
val cast_universe_level : int -> int
