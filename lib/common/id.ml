(** This module specifies identifiers *)

(** An abstract type for identifiers*)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
  val compare : t -> t -> int
  val default : t

  (** Map of identifiers. *)
  module Map : Map.S with type key = t
end

(** A string instance of the ID abstract type *)
module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.equal
  let compare = String.compare
  let default = "_"

  module Map = Map.Make (String)
end

module Name : ID = String_id
