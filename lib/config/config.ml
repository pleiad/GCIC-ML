(** This module manages the configuration options for the interpreter *)

(** Reexports *)

module Flag = Flag
module Variant = Variant

(** Initial configuration options *)

(** Max fuel for reduction *)
let max_fuel : int ref = ref 10000

(** Sets the current active value for the supplied configuration flag *)
let set_flag =
  let open Flag in
  function
  | Variant v -> Variant.set_variant v
  | Fuel fuel -> max_fuel := fuel

(** Access to configuration options *)

(** GCIC variant *)

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
let product_universe_level = Variant.product_universe_level

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
let cast_universe_level = Variant.cast_universe_level

(** Reduction fuel *)

(** Get initial fuel for reduction *)
let initial_fuel () = !max_fuel

(** Checks if reduction uses fuel. Reduction may diverge if false *)
let uses_fuel () = !max_fuel > 0
