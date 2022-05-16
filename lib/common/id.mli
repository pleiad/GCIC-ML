(** This module specifies identifiers *)

(** An abstract type for identifiers*)
module type ID = sig
    type t
  
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
    val compare: t -> t -> int
  end
  
  module Name : ID