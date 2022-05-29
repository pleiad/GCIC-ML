(** Configuration flags *)
type t =
  | Variant of Variant.t
  | Fuel of int

val to_string : t -> string
val ( = ) : t -> t -> bool
