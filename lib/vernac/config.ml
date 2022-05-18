(** COnfiguration options *)
type t =
  | Variant of Kernel.Variant.t
  | Fuel of int

let to_string = function
  | Variant v -> "Variant " ^ Kernel.Variant.to_string v
  | Fuel i -> "Fuel " ^ string_of_int i

let ( = ) cfg1 cfg2 =
  match cfg1, cfg2 with
  | Variant v1, Variant v2 -> v1 = v2
  | Fuel i, Fuel j -> i = j
  | _ -> false
