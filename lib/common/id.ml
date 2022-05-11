(** This module specifies identifiers *)

(** An abstract type for identifiers*)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

(** A string instance of the ID abstract type *)
module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.equal
end

module Name : ID = String_id