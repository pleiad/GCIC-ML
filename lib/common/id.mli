(** This module specifies identifiers *)

(** An abstract type for identifiers*)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
  val compare : t -> t -> int
  val default : t

  (** Map of identifiers. 
      Following Coq's approach  (https://github.com/coq/coq/blob/master/kernel/names.mli#L68) *)
  module Map : Map.S with type key = t
end

module Name : ID
