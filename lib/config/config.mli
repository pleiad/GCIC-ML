(* Reexports *)
module Flag = Flag
module Variant = Variant

(** Set flag *)
val set_flag : Flag.t -> unit

(** Flag getters *)

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
