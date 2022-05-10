(** This module specifies the structure of Contexts *)

type ('k, 'v) t

val empty : ('k, 'v) t
(** The empty context *)

val is_empty : ('k, 'v) t -> bool
(** Returns whether the context is empty *)

val add : key:'k -> value:'v -> ('k, 'v) t -> ('k, 'v) t
(** Adds a new declaration to the context *)

val lookup : key:'k -> ctx:('k, 'v) t -> 'v option
(** Returns the value associated to the given key. *)

val remove : key:'k -> ctx:('k, 'v) t -> ('k, 'v) t
(** Remove a declaration from the context *)

val to_list : ('k, 'v) t -> ('k * 'v) list
(** Converts a context into a list of key-value pairs *)

val of_list : ('k * 'v) list -> ('k, 'v) t
(** Converts a list of key-value pairs into a context *)

val to_string : ('k -> string) -> ('v -> string) -> ('k, 'v) t -> string