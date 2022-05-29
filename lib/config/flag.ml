(** This module specifies the possible configuration flags that can be set using the `set` command *)

(** Configuration flags *)
type t =
  | Variant of Variant.t
  | Fuel of int

(** Returns the stringified version of a configuration flag *)
let to_string = function
  | Variant v -> "Variant " ^ Variant.to_string v
  | Fuel i -> "Fuel " ^ string_of_int i

(** Identity predicate for configuration flags *)
let ( = ) cfg1 cfg2 =
  match cfg1, cfg2 with
  | Variant v1, Variant v2 -> v1 = v2
  | Fuel i, Fuel j -> i = j
  | _ -> false
