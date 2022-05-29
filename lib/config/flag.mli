(** This module specifies the possible configuration flags that can be set using the `set` command *)

(** Configuration flags *)
type t =
  | Variant of Variant.t (** The GCIC variant *)
  | Fuel of int (** Fuel for reduction *)

(** Returns the stringified version of a configuration flag *)
val to_string : t -> string

(** Identity predicate for configuration flags *)
val ( = ) : t -> t -> bool
