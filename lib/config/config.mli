(** This module manages the configuration options for the interpreter *)

(** Reexports *)

module Flag = Flag
module Variant = Variant

(** Sets the current active value for the supplied configuration flag *)
val set_flag : Flag.t -> unit

(** Access to configuration options *)

(** GCIC variant *)

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
val product_universe_level : int -> int -> int

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
val cast_universe_level : int -> int

(** Reduction fuel *)

(** Get initial fuel for reduction *)
val initial_fuel : unit -> int

(** Checks if reduction uses fuel. Reduction may diverge if false *)
val uses_fuel : unit -> bool
