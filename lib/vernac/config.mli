(** Configuration options *)
type t =
  | Variant of Kernel.Variant.t
  | Fuel of int

(** Returns the stringified version of a config *)
val to_string : t -> string

(** Equality predicate for config *)
val ( = ) : t -> t -> bool
