(** This module specifies the different GCIC variants and 
    the interface surrounding them *)

(** GCIC variants: Gradual, Normalizing and Shift *)
type t =
  | G
  | N
  | S

let to_string = function
  | G -> "G"
  | N -> "N"
  | S -> "S"

(** Parameter specifying the GCIC variant *)
let variant : t ref = ref N

let set_variant v = variant := v

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
let product_universe_level i j =
  match !variant with
  | G | N -> max i j
  | S -> max i j + 1

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
let cast_universe_level i =
  match !variant with
  | G -> i
  | N | S -> i - 1
