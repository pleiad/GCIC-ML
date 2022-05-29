module Flag = Flag
module Variant = Variant

(* Initial values for flags *)
(** Max fuel for reduction *)
let max_fuel : int ref = ref 10000

(** Set flag *)
let set_flag = let open Flag in
function
| Variant v -> Variant.set_variant v
| Fuel fuel -> max_fuel := fuel

(* Flag getters *)
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