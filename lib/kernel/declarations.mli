(** This module specifies global declarations. 
    It follows Coq's example a bit (https://github.com/coq/coq/blob/master/kernel/declarations.ml)    

    The global declarations are parameterized in order to maintain both the 
    pre-elaborated form of a term, as well as it's "cached" elaborated form.
*)
open Common.Id

(** Global declaration of constants *)
type 'a const_decl =
  { name : Name.t
  ; ty : 'a
  ; term : 'a
  }

(** Global declaration of inductives *)
type 'a ind_decl =
  { name : Name.t
  ; params : (Name.t * 'a) list
  ; sort : 'a
  ; level : int
  ; ctors : Name.t list
  }

(** Global declaration of constructor *)
type 'a ctor_decl =
  { name : Name.t
  ; ind : Name.t
  ; params : (Name.t * 'a) list
  ; args : (Name.t * 'a) list
  ; ty : 'a
  }

(** Module specifying a store of global declarations *)
module type Store = sig
  (** Type of global declaration *)
  type t

  (** Type of cached global declaration *)
  type cached_t

  (** Find a cached global declaration. *)
  val find : Name.t -> cached_t

  (** Add a global declaration. *)
  val add : Name.t -> t -> unit

  (** Checks if the declaration exists. *)
  val exists : Name.t -> bool

  (** Cache the declaration using the current variant *)
  val add_cache : Name.t -> cached_t -> unit
end

(** Module specifying a store factory *)
module Make_Store (D : sig
  type t
  type cached_t
end) : Store with type t = D.t with type cached_t = D.cached_t
