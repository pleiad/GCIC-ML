(** This module specifies the context for CastCIC (ie. Gamma) *)

type 'a t

val empty : 'a t
val add : Id.Name.t -> 'a -> 'a t -> 'a t
val find : Id.Name.t -> 'a t -> 'a
val lookup : Id.Name.t -> 'a t -> 'a option
val map : ('a -> 'b) -> 'a t -> 'b t
