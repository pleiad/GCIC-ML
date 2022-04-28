(** This module specifies the structure of Contexts *)

type ('k, 'v) t

val empty : ('k, 'v) t
(** The empty context *)

val is_empty : ('k, 'v) t -> bool
(** Returns whether the context is empty *)

val add : key:'k -> value:'v -> ('k, 'v) t -> ('k, 'v) t
(** Adds a new declaration to the context *)

val lookup : key:'k -> ctx:('k, 'v) t -> 'v
(** Returns the value associated to the given key.
      @raise Not_found if the given key has no associated value. *)
